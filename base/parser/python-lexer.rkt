#lang racket

(require racket/generator
         parser-tools/lex
         ragg/support
         (prefix-in : parser-tools/lex-sre)
         "unicode-char-names.rkt")

#| 
TODO: Scan first two physical lines for encoding.
TODO: Support other encodings (than utf-8).

'Logical' refers *here* to the spec's logical newlines (NEWLINE) as well as INDENT/DEDENT tokens.
'Physical' refers *here* to PHYSICAL-NEWLINE and WHITESPACE tokens. 


|#

(provide lex-all get-python-lexer)

(define (lex-all port)
  (local [(define (lex-acc acc python-lexer)
            (let ((token (python-lexer)))
              (if (equal? 
                   (cond 
                    [(position-token? token) (token-struct-type (position-token-token token))]
                    [(token-struct? token) (token-struct-type token)])
                   'EOF)
                  acc
                  (lex-acc (cons token acc) python-lexer))))]
         (reverse (lex-acc (list) (get-python-lexer port)))))

#| LEXER PARTS AND RELATED FUNCTIONS |#

#| Strings are detected by the start and then parsed by a separate lexer. The rest are transliterated from the Python spec and then paired with re-parsing functions. |#
(define-lex-abbrevs
  (physical-eol (:or "\n" "\r\n" "\r"))
  (begin-string (:: (:? (:or "b" "B")) (:? (:or "r" "R")) (:or "'" "\"" "'''" "\"\"\""))))

#| NAMES |#

#| 
Identifiers
Regarding unicode: catch all unicode non-ascii characters in the lexer and test explicitly. 
This only works because there are no valid source chars outside the ASCII range except in identifiers and strings.
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

;; Char c in the set abfnrtv to escape character of \c
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

(define (octal-chars->char . chars)
  (integer->char (string->number (apply string chars) 8)))

(define (hex-chars->char . chars)
  (integer->char (string->number (apply string chars) 16)))

(define (character-name->char name)
  (hash-ref unicode-char-names name 
            (lambda () (error (string-append "Unicode character name not found: " name "\n")))))

;; TODO: Catch and rethrow errors as syntax errors
(define (backslash-escaped lexeme unicode?)
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
               (escape rest (cons (hex-chars->char c1 c2) acc))]
              [(? (lambda _ unicode?) (list-rest #\\ #\u c1 c2 c3 c4 rest))
               (escape rest (cons (hex-chars->char c1 c2 c3 c4) acc))]
              [(? (lambda _ unicode?) (list-rest #\\ #\U c1 c2 c3 c4 c5 c6 c7 c8 rest))
               (escape rest (cons (hex-chars->char c1 c2 c3 c4 c5 c6 c7 c8) acc))]
              [(? (lambda _ unicode?)
                  (list #\\ #\N #\{ (and (not #\}) name-chars) ... #\} rest ...))
               (escape rest (cons (character-name->char (apply string name-chars)) acc))]
              [(list-rest c rest) (escape rest (cons c acc))])))
         (list->string (escape (string->list lexeme) (list)))))

#| BYTESTRINGS |#
(define (unescaped l)
  (regexp-replace* #rx"[\\]" l "\\\\\\\\"))

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

#| TODO: Optional warning system for space/tab mixing |#
(define (count-spaces str)
  (foldl (lambda (char count)
           (+ count (case char
                      [(#\space) 1]
                      [(#\014) 0] 
                      [(#\tab) (- 8 (modulo count 8))])))
         0
         (string->list str)))

;; Lex string up to endquotes and return content pair - port should start with string contents.
;; quote-char: #\' or #\"
;; quote-count: 1 or 3
(define (string-lexer port quote-char quote-count bytestring? raw? real-start-pos)
  ;; unquote-count: <= quote-count
  ;; lexeme-lst: reversed list of lexemes including separate endquotes
  (define (finish-string lexeme-lst end-pos)
    (let ((content-string (string-append* (reverse (if (equal? quote-count 3)
                                                        (cdddr lexeme-lst)
                                                        (cdr lexeme-lst))))))
      (token 'STRING 
             (cond [(and bytestring? raw?) (cons 'bytes (unescaped content-string))]
                   [bytestring? (cons 'bytes content-string)]
                   [raw? (cons 'string content-string)]
                   [else (cons 'string (backslash-escaped content-string #t))])
            #:line (position-line real-start-pos)
            #:column (position-col real-start-pos)
            #:offset (position-offset real-start-pos)
            #:span (- (position-offset end-pos) (position-offset real-start-pos)))))
  (define (continue-string port unquote-count lexeme-lst [last-end-pos #f])
    (define string-part-lex
      (lexer
       ("'" (continue-string port
                             (if (equal? quote-char #\') (add1 unquote-count) 0)
                             (cons lexeme lexeme-lst)
                             end-pos))
       ("\"" (continue-string port
                              (if (equal? quote-char #\") (add1 unquote-count) 0)
                              (cons lexeme lexeme-lst)
                              end-pos))
       ((:or "\\\"" "\\'")
        (continue-string port 0 (cons lexeme lexeme-lst)))
       ((:: "\\" physical-eol)
        (continue-string port 0 lexeme-lst))
       (any-char ;; Including backslashes alone
        (continue-string port 0 (cons lexeme lexeme-lst)))
       ((eof) (error "EOF in string"))))
    (if (equal? unquote-count quote-count)
        (finish-string lexeme-lst last-end-pos)
        (string-part-lex port)))
  (continue-string port 0 '()))

(define-syntax pos-token
  (syntax-rules ()
    ((_ sym val)
     (token sym val 
            #:line (position-line start-pos)
            #:column (position-col start-pos)
            #:offset (position-offset start-pos)
            #:span (- (position-offset end-pos) (position-offset start-pos))))))

(define comment-lexer
  (lexer
   (physical-eol (pos-token 'PHYSICAL-NEWLINE "PHYSICAL-NEWLINE"))
   (any-char (comment-lexer input-port))))

;; Physical lexer 
(define lex
  (lexer
   ("#" (comment-lexer input-port))
   ((:or "class" "finally" "is" "return" "continue" "for" "lambda" "try" "def" "from" "nonlocal" "while" "and" "del" "global" "not" "with" "as" "elif" "if" "or" "yield" "assert" "else" "import" "pass" "break" "except" "in" "raise" 
         "..."
         "->"
         "+" "-" "*" "**" "/" "//" "%" "<<" ">>" "&" "|" "^" "~" "<" ">" "<=" ">=" "==" "!=" 
         "(" ")" "[" "]" "{" "}" "," ":" "." ";" "@" "=" "+=" "-=" "*=" "/=" "//=" "%=" "&=" "|=" "^=" ">>=" "<<=" "**=") 
    (pos-token (string->symbol lexeme) lexeme))
   
   (integer (pos-token 'NUMBER (cons 'integer (parse-integer lexeme))))
   (floatnumber (pos-token 'NUMBER (cons 'float (parse-float lexeme))))
   (imagnumber (pos-token 'NUMBER (cons 'imaginary lexeme)))

   (begin-string 
    (match lexeme
      [(regexp #rx"^(b|B)(r|R)(\"\"\"|''')")
       (string-lexer input-port (string-ref lexeme 2) 3 #t #t start-pos)]
      [(regexp #rx"^(b|B)(\"\"\"|''')")
       (string-lexer input-port (string-ref lexeme 1) 3 #t #f start-pos)]
      [(regexp #rx"^(b|B)(r|R)[\"']")
       (string-lexer input-port (string-ref lexeme 2) 1 #t #t start-pos)]
      [(regexp #rx"^(b|B)[\"']")
       (string-lexer input-port (string-ref lexeme 1) 1 #t #f start-pos)]
      [(regexp #rx"^(r|R)(\"\"\"|''')")
       (string-lexer input-port (string-ref lexeme 1) 3 #f #t start-pos)]
      [(regexp #rx"^(\"\"\"|''')")
       (string-lexer input-port (string-ref lexeme 0) 3 #f #f start-pos)]
      [(regexp #rx"^(r|R)[\"']")
       (string-lexer input-port (string-ref lexeme 1) 1 #f #t start-pos)]
      [(regexp #rx"^[\"']") 
       (string-lexer input-port (string-ref lexeme 0) 1 #f #f start-pos)]))

   (identifier (if (valid-identifier? lexeme)
                   (pos-token 'NAME (cons 'name lexeme)) ; Not sure whether these should be normalized.
                   (error (string-append "Invalid unicode identifier: " lexeme))))

   ((:: "\\" physical-eol) (lex input-port))
   (physical-eol (pos-token 'PHYSICAL-NEWLINE "PHYSICAL-NEWLINE"))
   ((:+ (:or " " "\t" "\f")) (pos-token 'WHITESPACE (count-spaces lexeme)))
   ((eof) (token 'EOF "EOF"))))

;; Rewrite a token's type and content to symbol name, copying source position
(define (rename-token t name)
  (token name (symbol->string name)
         #:offset (token-struct-offset t)
         #:line (token-struct-line t)
         #:column (token-struct-column t)
         #:span (token-struct-span t)))
  
;; Logical lexer - produces logical/other tokens using physical lexer
(define (get-python-lexer input-port)
  (port-count-lines! input-port)
  (generator 
   () 
   (local ((define (next-token) 
             (let* ((physical-token (lex input-port)))
               (values physical-token (token-struct-type physical-token) (token-struct-val physical-token))))
           ;; Adjust brace depth for any physical token
           ;; Actual matching occurs in the parser; this just provides logical lines for valid programs
           (define (adjust-depth depth t-type)
             (case t-type 
               [(\( \[ \{) (+ depth 1)]
               [(\) \] \}) (if (< depth 1) 
                               (error "Brace depth < 0")
                               (- depth 1))]
               [else depth]))

           ;; Taking indent stack and latest amount of significant ws, 
           ;; yield all necessary indent/dedent tokens and return new indent stack
           ;; if last-newline is #f instead of a token, signal an error (this is more a parser issue, but to keep it simple...)
           (define (adjust-indent-stack indent ws-amount last-newline)
             (cond [(= ws-amount (car indent)) indent]
                   [(> ws-amount (car indent)) 
                    (begin 
                      (unless last-newline
                        (error "First line cannot be indented. (No newline while adjusting indentation.)"))
                      (yield (rename-token last-newline 'INDENT)) 
                      ;; The newline isn't a great position source for INDENTs, but indent positions 
                      ;; probably won't be a major source of frustration.
                      (cons ws-amount indent))]
                   [(< ws-amount (car indent))
                    (let ((new-stack (cdr indent)))
                      (if (> ws-amount (car indent))
                          (error "Bad indent")
                          (begin 
                            (unless last-newline
                              (error "Firstq line cannot be indented. (No newline while adjusting indentation.)"))
                            (yield (rename-token last-newline 'DEDENT))
                            (adjust-indent-stack new-stack ws-amount last-newline))))]))

           #|
           Something like a three state machine plus some arbitrary data per state.
           (begin-line and after are really two states, one with #f for last-newline until 
           slurg gets a PHYSICAL-NEWLINE, with error signalled in adjust-indent-stack)
           
           States: begin-line, after-ws, slurg
           Input set: PHYSICAL-NEWLINE, WHITESPACE, EOF, other

           indent: Indent list, strictly decreasing from top to bottom
           last-newline: The last physical newline from slurg or #f. Used for source position in INDENT/DEDENT tokens.
           last-t: Similar to last-newline, for the edge case of EOF in slurg.
           ws-amount: integer size of last whitespace token
           brace-depth: integer number of open braces of *any* kind, closed by *any* kind
           |#

           (define (begin-line indent last-newline)
             (let-values (((t t-type t-val) (next-token)))
               (case t-type
                 [(PHYSICAL-NEWLINE) (begin-line indent last-newline)]
                 [(WHITESPACE) (after-ws indent t-val last-newline)]
                 [(EOF) (begin 
                          (adjust-indent-stack indent 0 last-newline)
                          (yield (token 'EOF "EOF")))]
                 [else (let ((new-indent (adjust-indent-stack indent 0 last-newline)))
                         (begin (yield t)
                                (slurg new-indent (adjust-depth 0 t-type) t)))])))
             
           (define (after-ws indent ws-amount last-newline)
             (let-values (((t t-type t-val) (next-token)))
               (case t-type
                 [(PHYSICAL-NEWLINE) (begin-line indent last-newline)]
                 [(WHITESPACE) (error "Unexpected second whitespace token")]
                 [(EOF) (begin (adjust-indent-stack indent 0)
                               (yield (token 'EOF "EOF")))]
                 [else (let ((new-indent (adjust-indent-stack indent ws-amount last-newline)))
                         (begin (yield t)
                                (slurg new-indent (adjust-depth 0 t-type) t)))])))

           ;; brace-depth is number >= 0. Remain in slurg while brace-depth > 0
           (define (slurg indent brace-depth last-t)
             (let-values (((t t-type t-val) (next-token)))
               (case t-type
                 [(EOF) (if (> brace-depth 0) (error "EOF inside braces")
                            (begin (yield (rename-token last-t 'NEWLINE)) ;; last-t: Author feels lazy; just copy last real token.
                                   (adjust-indent-stack indent 0 last-t) 
                                   (yield (token 'EOF "EOF"))))]
                 [(PHYSICAL-NEWLINE)
                  (if (> brace-depth 0)
                      (slurg indent brace-depth t)
                      (begin (yield (rename-token t 'NEWLINE))
                             (begin-line indent t)))] ;; Note: This is where actual last-newline originates.
                 [(WHITESPACE) (slurg indent brace-depth t)]
                 [else (begin (yield t)
                              (slurg indent (adjust-depth brace-depth t-type) t))]))))
          (begin-line '(0) #f))))
