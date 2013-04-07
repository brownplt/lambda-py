#lang racket

(require racket/generator
         parser-tools/lex
         ragg/support
         (prefix-in : parser-tools/lex-sre))


#| TODO: Scan first two physical lines for encoding. |#
#| TODO: Support other encodings (than utf-8). |#

#| 'Logical' refers *here* to the spec's logical newlines (NEWLINE) as well as INDENT/DEDENT tokens. |#
#| 'Physical' refers *here* to PHYSICAL-NEWLINE and WHITESPACE tokens. |#

(provide lex-all get-python-lexer)

(define (lex-all port)
  (local [(define (lex-acc acc python-lexer)
            (let ((token (python-lexer)))
              (if (equal? (token-struct-type token) 'EOF)
                  acc
                  (lex-acc (cons token acc) python-lexer))))]
         (reverse (lex-acc (list) (get-python-lexer port)))))

#| LEXER PARTS AND RELATED FUNCTIONS |#

#| These are mostly transliterated from the Python spec and then (unfortunately) paired with re-parsing functions. |#

(define-lex-abbrevs
  (physical-eol (:or "\n" "\r\n" "\r")))

#| NAMES |#

#| 
Catch all unicode non-ascii characters and test explicitly. 
This only works because there are no valid source chars outside the ASCII range except in identifiers.
|#
(define-lex-abbrevs
  (identifier (:: identifier-start (:* identifier-continue)))
  (identifier-continue (:or (char-range "0" "9") (char-range "a" "z") (char-range "A" "Z") "_" (char-range "\U000080" "\U10FFFF")))
  (identifier-start (:or (char-range "a" "z") (char-range "A" "Z") "_" (char-range "\U000080" "\U10FFFF"))))

#| See Python 3 and PEP 3131 |#
#| TODO: Test. (At all.) |#
(define (valid-python-id-start-char? c)
  (or (member (char-general-category c) '(lu ll lt lm lo nl))
      (member c '(#\_ #\u2118 #\u212E #\u309B #\u309C))))

(define (valid-python-id-char? c)
  (or (valid-python-id-start-char? c)
      (member (char-general-category c) '(mn mc nd pc))
      (member c '(#\u00B7 #\u0387  #\u1369 #\u136A #\u136B #\u136C #\u136D #\u136E #\u136F #\u1370 #\u1371 #\u19DA))))

(define (valid-identifier? lexeme)
  (let ((normalized-lexeme (string-normalize-nfkc lexeme)))
    (and (valid-python-id-start-char? (string-ref normalized-lexeme 0))
         (andmap valid-python-id-char? (string->list normalized-lexeme)))))

#| STRINGS |#
(define-lex-abbrevs
  (stringliteral (:: (:? stringprefix) (:or shortstring longstring)))
  (stringprefix (:or "r" "R"))
  (shortstring (:or (:: "'" (:* single-shortstringitem) "'") (:: "\"" (:* double-shortstringitem) "\"")))
  (longstring (:or (:: "'''" (:- (:* longstringitem) (:: (:* any-char) "'''" (:* any-char))) "'''") (:: "\"\"\"" (:- (:* longstringitem) (:: (:* any-char) "\"\"\"" (:* any-char))) "\"\"\"")))
  (single-shortstringitem (:or (:~ "'" "\\" "\n" "\r") stringescapeseq))
  (double-shortstringitem (:or (:~ "\"" "\\" "\n" "\r") stringescapeseq))
  (longstringitem (:or (:~ "\\") stringescapeseq))
  (stringescapeseq (:: "\\" any-char)))

;; TODO: Unicode escapes
(define (parse-string lexeme)
  (let* ((raw (equal? "r" (substring lexeme 0 1)))
         (lexeme-noraw (substring lexeme (if raw 1 0)))
         (triple (and
                  (> (string-length lexeme-noraw) 2)
                  (char=? (string-ref lexeme-noraw 0)
                          (string-ref lexeme-noraw 1) 
                          (string-ref lexeme-noraw 2))))
         (lexeme-no-quotes (substring lexeme-noraw 
                                      (if triple 3 1) 
                                      (- (string-length lexeme-noraw) (if triple 3 1)))))
    (if raw lexeme-no-quotes (backslash-escaped lexeme-no-quotes))))

;; Char c in the set abfnrtv to escaped character of \c
(define (escape-char c)
  (match c
    [#\a #\007] ; bell
    [#\b #\010] ; backspace
    [#\f #\014]
    [#\n #\012]
    [#\r #\015] 
    [#\t #\tab]
    [#\v #\vtab]))

(define (octal-char? c)
  (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)))

;; Turn three octal chars into a one-byte char
(define (octal-chars->char c1 c2 c3)
  (integer->char (string->number (string c1 c2 c3) 8)))

(define (hex-chars->char c1 c2)
  (integer->char (string->number (string c1 c2) 16)))

(define (backslash-escaped lexeme)
  (local ((define (escape char-lst acc)
            (match char-lst
              [`() (reverse acc)]
              [`(#\\) (error (string-append "String constant ends with backslash: " lexeme))]
              [(list-rest #\\ #\newline rest) (escape rest acc)]
              [(list-rest #\\ (and c (or #\\ #\' #\")) rest) 
               (escape rest (cons c acc))]
              [(list-rest #\\ (and c (or #\a #\b #\f #\n #\r #\t #\v)) rest)
               (escape rest (cons (escape-char c) acc))]
              [(list-rest #\\ (? octal-char? c1) (? octal-char? c2) (? octal-char? c3) rest)
               (escape rest (cons (octal-chars->char c1 c2 c3) acc))]
              [(list-rest #\\ (? octal-char? c1) (? octal-char? c2) rest)
               (escape rest (cons (octal-chars->char #\0 c1 c2) acc))]
              [(list-rest #\\ (? octal-char? c1) rest)
               (escape rest (cons (octal-chars->char #\0 #\0 c1) acc))]
              [(list-rest #\\ #\x c1 c2 rest)
               ;; TODO: Catch and rethrow errors as syntax errors
               (escape rest (cons (hex-chars->char c1 c2) acc))]
              [(list-rest c rest) (escape rest (cons c acc))])))
         
         (list->string (escape (string->list lexeme) (list)))))

#| BYTESTRINGS |#
(define-lex-abbrevs
  (bytesliteral (:: bytesprefix (:or shortbytes longbytes)))
  (bytesprefix (:or "b" "B" "br" "Br" "bR" "BR"))
  (shortbytes (:or (:: "'" (:* single-shortbytesitem) "'") (:: "\"" (:* double-shortbytesitem) "\"")))
  (longbytes (:or (:: "'''" (:* longbytesitem) "'''") (:: "\"\"\"" (:* longbytesitem) "\"\"\"")))
  (single-shortbytesitem (:or single-shortbyteschar bytesescapeseq))
  (double-shortbytesitem (:or double-shortbyteschar bytesescapeseq))
  (longbytesitem (:or longbyteschar bytesescapeseq))
  (single-shortbyteschar (:- (char-range "\u0" "\u7f") (:or "\\" "\n" "\r" "'")))
  (double-shortbyteschar (:- (char-range "\u0" "\u7f") (:or "\\" "\n" "\r" "\"")))
  (longbyteschar (:- (char-range "\u0" "\u7f") "\\"))
  (bytesescapeseq (:: "\\" (char-range "\u0" "\u7f"))))

(define (parse-bytestring lexeme)
  (let-values ([(raw? lead-chars follow-chars)
                (match lexeme
                  [(regexp #rx"^(b|B)(r|R)[\"'][\"'][\"']") (values #t 5 3)]
                  [(regexp #rx"^(b|B)[\"'][\"'][\"']") (values #f 4 3)]
                  [(regexp #rx"^(b|B)(r|R)[\"']") (values #t 3 1)]
                  [(regexp #rx"^(b|B)[\"']") (values #f 2 1)])])
    (let ((unescaped-content (substring lexeme lead-chars (- (string-length lexeme) follow-chars))))
      (if raw? 
          ;; \n -> \\n ("\\n" -> "\\\\n")
          (regexp-replace* #rx"[\\]" unescaped-content "\\\\\\\\")
          unescaped-content))))

#| INTEGERS |#
(define-lex-abbrevs
  (integer (:or decimalinteger octinteger hexinteger bininteger))
  (decimalinteger (:or (:: nonzerodigit (:* digit)) (:+ "0")))
  (nonzerodigit (char-range "1" "9"))
  (digit (char-range "0" "9"))
  (octinteger (:: "0" (:or "o" "O") (:+ octdigit)))
  (hexinteger (:: "0" (:or "x" "X") (:+ hexdigit)))
  (bininteger (:: "0" (:or "b" "B") (:+ bindigit)))
  (octdigit (char-range "0" "7"))
  (hexdigit (:or digit (char-range "a" "f") (char-range "A" "F")))
  (bindigit (:or "0" "1")))

(define (parse-integer lexeme)
  (let* ((has-radix (and (> (string-length lexeme) 2) (not (char-numeric? (string-ref lexeme 1)))))
         (radix (if (not has-radix) 10
                    (case (char-downcase (string-ref lexeme 1))
                      [(#\x) 16]
                      [(#\o) 8]
                      [(#\b) 2]))))
    (string->number (if (not has-radix) lexeme
                        (substring lexeme 2))
                    radix)))
#| FLOATS |#
(define-lex-abbrevs
  (floatnumber (:or pointfloat exponentfloat))
  (pointfloat (:or (:: (:? intpart) fraction) (:: intpart ".")))
  (exponentfloat (:: (:or intpart pointfloat) exponent))
  (intpart (:+ digit))
  (fraction (:: "." (:+ digit)))
  (exponent (:: (:or "e" "E") (:? (:or "+" "-")) (:+ digit))))

(define parse-float string->number)

#| IMAGINARIES |#
(define-lex-abbrevs
  (imagnumber (:: (:or floatnumber intpart) (:or "j" "J"))))

(define (parse-imaginary lexeme)
  (* 0+1i (string->number (substring lexeme 0 (- (string-length lexeme) 1)))))

#| TODO: Optional warning system for space/tab mixing |#
(define (count-spaces str)
  (foldl (lambda (char count)
           (+ count (if (eq? char #\space) 
                        1
                        (- 8 (modulo count 8)))))
         0
         (string->list str)))

(define comment-lexer
  (lexer
   (physical-eol (token 'PHYSICAL-NEWLINE))
   (any-char (comment-lexer input-port))))

#|
Simple lexer, produces physical/other tokens.
|#
(define lex
  (lexer 
   ("#" (comment-lexer input-port))

   ((:or "class" "finally" "is" "return" "continue" "for" "lambda" "try" "def" "from" "nonlocal" "while" "and" "del" "global" "not" "with" "as" "elif" "if" "or" "yield" "assert" "else" "import" "pass" "break" "except" "in" "raise" 
         "+" "-" "*" "**" "/" "//" "%" "<<" ">>" "&" "|" "^" "~" "<" ">" "<=" ">=" "==" "!=" 
         "(" ")" "[" "]" "{" "}" "," ":" "." ";" "@" "=" "+=" "-=" "*=" "/=" "//=" "%=" "&=" "|=" "^=" ">>=" "<<=" "**=") 
    (token (string->symbol lexeme) lexeme))
   
   (integer (token 'NUMBER (cons 'integer (parse-integer lexeme))))
   (floatnumber (token 'NUMBER (cons 'float (parse-float lexeme))))
   (imagnumber (token 'NUMBER (cons 'imaginary lexeme)))
   (stringliteral (token 'STRING (cons 'string (parse-string lexeme))))
   (bytesliteral (token 'STRING (cons 'bytes (parse-bytestring lexeme))))

   (identifier (if (valid-identifier? lexeme)
                   (token 'NAME (cons 'name lexeme)) ; Not sure whether these should be normalized.
                   (error (string-append "Invalid unicode identifier: " lexeme))))

   ((:: "\\" physical-eol) (lex input-port))
   (physical-eol (token 'PHYSICAL-NEWLINE))
   ((:+ (:or " " "\t")) (token 'WHITESPACE (count-spaces lexeme)))
   ((eof) (token 'EOF "EOF"))))

#| Logical lexer - produces logical/other tokens using physical lexer |#
(define (get-python-lexer input-port)
  (generator 
   () 
   (local ((define (next-token) 
             (let ((physical-token (lex input-port)))
               (values physical-token (token-struct-type physical-token) (token-struct-val physical-token))))
           ;; adjust brace depth for any physical token
           (define (adjust-depth depth t-type)
             (case t-type 
               [(\( \[ \{) (+ depth 1)]
               [(\) \] \}) (if (< depth 1) 
                               (error "Brace depth < 0")
                               (- depth 1))]
               [else depth]))

           ;; Taking indent stack and latest amount of significant ws, 
           ;; yield all necessary indent/dedent tokens and return new indent stack
           (define (adjust-indent-stack indent ws-amount)
             (cond [(= ws-amount (car indent)) indent]
                   [(> ws-amount (car indent)) 
                    (begin (yield (token 'INDENT "INDENT"))
                           (cons ws-amount indent))]
                   [(< ws-amount (car indent))
                    (let ((new-stack (cdr indent)))
                      (if (> ws-amount (car indent))
                          (error "Bad indent")
                          (begin (yield (token 'DEDENT "DEDENT"))
                                 (adjust-indent-stack new-stack ws-amount))))]))

           (define (begin-line indent)
             (let-values (((t t-type t-val) (next-token)))
               (case t-type
                 [(PHYSICAL-NEWLINE) (begin-line indent)]
                 [(WHITESPACE) (after-ws indent t-val)]
                 [(EOF) (begin 
                          (adjust-indent-stack indent 0)
                          (yield (token 'EOF "EOF")))]
                 [else (let ((new-indent (adjust-indent-stack indent 0)))
                         (begin (yield t)
                                (slurg new-indent (adjust-depth 0 t-type))))])))
             
           (define (after-ws indent ws-amount)
             (let-values (((t t-type t-val) (next-token)))
               (case t-type
                 [(PHYSICAL-NEWLINE) (begin-line indent)]
                 [(WHITESPACE) (error "Unexpected second whitespace token")]
                 [(EOF) (begin (adjust-indent-stack indent 0)
                               (yield (token 'EOF "EOF")))]
                 [else (let ((new-indent (adjust-indent-stack indent ws-amount)))
                         (begin (yield t)
                                (slurg new-indent (adjust-depth 0 t-type))))])))

           ;; brace-depth is number >= 0. Remain in slurg while brace-depth > 0
           (define (slurg indent brace-depth)
             (let-values (((t t-type t-val) (next-token)))
               (case t-type
                 [(EOF) (if (> brace-depth 0) (error "EOF inside braces")
                            (begin (yield (token 'NEWLINE "NEWLINE"))
                                   (adjust-indent-stack indent 0)
                                   (yield (token 'EOF "EOF"))))]
                 [(PHYSICAL-NEWLINE)
                  (if (> brace-depth 0)
                      (slurg indent brace-depth)
                      (begin (yield (token 'NEWLINE "NEWLINE"))
                             (begin-line indent)))]
                 [(WHITESPACE) (slurg indent brace-depth)]
                 [else (begin (yield t)
                              (slurg indent (adjust-depth brace-depth t-type)))]))))
          (begin-line '(0)))))
