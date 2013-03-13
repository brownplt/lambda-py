#lang racket

(require parser-tools/lex
	 parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

#| TODO: Scan first two physical lines for encoding. |#
#| TODO: Support other encodings (than utf-8). |#

#| 'Logical' refers *here* to the spec's logical newlines as well as INDENT/DEDENT tokens. |#
#| 'Physical' refers *here* to physical newline and whitespace tokens. |#

(provide lex-all get-python-lexer)

(define (lex-all port)
  (local [(define (lex-acc port acc python-lexer)
	    (let ((token (python-lexer port)))
	      (if (equal? (token-name token) 'EOF)
		  acc
		  (lex-acc port (cons token acc) python-lexer))))]
	 (reverse (lex-acc port (list) (get-python-lexer)))))

#| LEXER PARTS AND RELATED FUNCTIONS |#

#| These are mostly transliterated from the Python spec and then (unfortunately) paired with re-parsing functions. |#

(define-lex-abbrevs
  (physical-eol (:or "\n" "\r\n" "\r"))
  (line-continue (:: "\\" physical-eol)))

#| IDENTIFIERS |#

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
  (longstring (:or (:: "'''" (:* longstringitem) "'''") (:: "\"\"\"" (:* longstringitem) "\"\"\"")))
  (single-shortstringitem (:or (:~ "'" "\\" "\n" "\r") stringescapeseq))
  (double-shortstringitem (:or (:~ "\"" "\\" "\n" "\r") stringescapeseq))
  (longstringitem (:or (:~ "\\") stringescapeseq))
  (stringescapeseq (:: "\\" any-char)))

(define (parse-string lexeme)
  (let* ((raw (eq? "r" (substring lexeme 0 1)))
	 (lexeme-noraw (substring lexeme (if raw 1 0)))
	 (triple (eq? (substring lexeme-noraw 0 1) (substring lexeme-noraw 1 2)))
	 (lexeme-no-quotes (substring lexeme-noraw 
				     (if triple 3 1) 
				     (- (string-length lexeme-noraw) (if triple 3 1)))))
    (if raw lexeme-no-quotes (backslash-escaped lexeme-no-quotes))))

; TODO: Escapes
(define (backslash-escaped lexeme)
  lexeme)

#| BYTESTRINGS |#
(define-lex-abbrevs
  (bytesliteral (:: bytesprefix (:or shortbytes longbytes)))
  (bytesprefix (:or "b" "B" "br" "Br" "bR" "BR"))
  (shortbytes (:or (:: "'" (:* single-shortbytesitem) "'") ("\"" (:* double-shortbytesitem) "\"")))
  (longbytes (:or (:: "'''" (:* longbytesitem) "'''") ("\"\"\"" (:* longbytesitem) "\"\"\"")))
  (single-shortbytesitem (:or single-shortbyteschar bytesescapeseq))
  (double-shortbytesitem (:or double-shortbyteschar bytesescapeseq))
  (longbytesitem (:or longbyteschar bytesescapeseq))
  (single-shortbyteschar (:- (char-range "\u0" "\u7f") (:or "\\" "\n" "\r" "'")))
  (double-shortbyteschar (:- (char-range "\u0" "\u7f") (:or "\\" "\n" "\r" "\"")))
  (longbyteschar (:- (char-range "\u0" "\u7f") "\\"))
  (bytesescapeseq (:: "\\" (char-range "\u0" "\u7f"))))

(define (parse-bytestring lexeme)
  (let* ((raw (member (string-ref lexeme 1) '(#\r #\R)))
	 (lexeme-no-prefix (substring lexeme (if raw 2 1)))
	 (triple (eq? (substring lexeme-no-prefix 0 1) (substring lexeme-no-prefix 1 2)))
	 (lexeme-no-quotes (substring lexeme-no-prefix 
				     (if triple 3 1) 
				     (- (string-length lexeme-no-prefix) (if triple 3 1)))))
    (if raw lexeme-no-quotes (backslash-escaped lexeme-no-quotes))))

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

#| TOKENS - For both lexer phases |#
(define-tokens nonempty-physical-tokens (WHITESPACE))
(define-tokens nonempty-other-tokens (IDENTIFIER INTEGER FLOAT IMAGINARY STRING BYTES))
(define-empty-tokens empty-physical-tokens (PHYSICAL-NEWLINE))
(define-empty-tokens empty-logical-tokens (INDENT DEDENT NEWLINE))
(define-empty-tokens empty-other-tokens (EOF

					 False      class      finally    is         return
					 None       continue   for        lambda     try
					 True       def        from       nonlocal   while
					 and        del        global     not        with
					 as         elif       if         or         yield
					 assert     else       import     pass
					 break      except     in         raise

					 +       -       *       **      /       //      %
					 <<      >>      &       BAR       ^       ~
					 <       >       <=      >=      ==      !=

					 LP       RP       LSB       RSB       LCB       RCB
					 COMMA       :      DOT        SEMICOLON       @       =
					 +=      -=      *=      /=      //=     %=
					 &=      BAR=      ^=      >>=     <<=     **= 
					 ))

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
   (physical-eol (token-PHYSICAL-NEWLINE))
   (any-char (comment-lexer input-port))))

#|
Simple lexer, produces physical/other tokens.
|#
(define lex
  (lexer 
   ("#" (comment-lexer input-port))

   ("False" (token-False))
   ("class" (token-class))
   ("finally" (token-finally))
   ("is" (token-is))
   ("return" (token-return))
   ("None" (token-None))
   ("continue" (token-continue))
   ("for" (token-for))
   ("lambda" (token-lambda))
   ("try" (token-try))
   ("True" (token-True))
   ("def" (token-def))
   ("from" (token-from))
   ("nonlocal" (token-nonlocal))
   ("while" (token-while))
   ("and" (token-and))
   ("del" (token-del))
   ("global" (token-global))
   ("not" (token-not))
   ("with" (token-with))
   ("as" (token-as))
   ("elif" (token-elif))
   ("if" (token-if))
   ("or" (token-or))
   ("yield" (token-yield))
   ("assert" (token-assert))
   ("else" (token-else))
   ("import" (token-import))
   ("pass" (token-pass))
   ("break" (token-break))
   ("except" (token-except))
   ("in" (token-in))
   ("raise" (token-raise))

   ("+" (token-+))
   ("-" (token--))
   ("*" (token-*))
   ("**" (token-**))
   ("/" (token-/))
   ("//" (token-//))
   ("%" (token-%))
   ("<<" (token-<<))
   (">>" (token->>))
   ("&" (token-&))
   ("|" (token-BAR))
   ("^" (token-^))
   ("~" (token-~))
   ("<" (token-<))
   (">" (token->))
   ("<=" (token-<=))
   (">=" (token->=))
   ("==" (token-==))
   ("!=" (token-!=))

   ("(" (token-LP))
   (")" (token-RP))
   ("[" (token-LSB))
   ("]" (token-RSB))
   ("{" (token-LCB))
   ("}" (token-RCB))
   ("," (token-COMMA))
   (":" (token-:))
   ("." (token-DOT))
   (";" (token-SEMICOLON))
   ("@" (token-@))
   ("=" (token-=))
   ("+=" (token-+=))
   ("-=" (token--=))
   ("*=" (token-*=))
   ("/=" (token-/=))
   ("//=" (token-//=))
   ("%=" (token-%=))
   ("&=" (token-&=))
   ("|=" (token-BAR=))
   ("^=" (token-^=))
   (">>=" (token->>=))
   ("<<=" (token-<<=))
   ("**=" (token-**=))
   
   (integer (token-INTEGER (parse-integer lexeme)))
   (floatnumber (token-FLOAT (parse-float lexeme)))
   (imagnumber (token-IMAGINARY (parse-imaginary lexeme)))
   (stringliteral (token-STRING (parse-string lexeme)))

   (identifier (if (valid-identifier? lexeme)
		   (token-IDENTIFIER lexeme) ; Not sure whether these should be normalized.
		   (error (string-append "Invalid unicode identifier: " lexeme))))

   ((:: "\\" physical-eol) (lex input-port))
   (physical-eol (token-PHYSICAL-NEWLINE))
   ((:+ (:or " " "\t")) (token-WHITESPACE (count-spaces lexeme)))
   ((eof) (token-EOF))))

#| Logical lexer - produces logical/other tokens using physical lexer |#
(define (get-python-lexer)
  (local (

	  #|
	  Traditional lexer function 'lex' will produce whitespace 
	  tokens on any whitespace outside comments, so lines will be like one of...
	  OTHER -> slurg (process indent for 0 spaces)
	  WHITESPACE -> OTHER -> slurg (indent on whitespace)
	  WHITESPACE (ignore)
	  ...where slurg is other and whitespace.
	  
	  begin-line: consume token and...
	  - PHYSICAL-NEWLINE: to begin-line
	  - WHITESPACE: to leading-ws, store spaces 
	  - OTHER: to pre-slurg storing physical token
	  leading-ws: consume token and...
	  - PHYSICAL-NEWLINE: to begin-line, zero spaces
	  - WHITESPACE: What?
	  - OTHER: to pre-slurg storing physical token
	  pre-slurg: Not really a state. For 'clarity', all indent/dedent and the associated stored-lexeme are given by this state, then go to slurg.
	  slurg: consume token and...
	  - PHYSICAL-NEWLINE: to begin-line, zero spaces, produce NEWLINE
	  - WHITESPACE: to slurg
	  - OTHER: to slurg, produce other
	  |#

	  (define brace-depth 0) ; TODO: Brace depth

	  (define state 'begin-line) ; lexer state. 
	  (define indent-stack '(0)) ; indent stack, strictly decreasing from top to bottom.
	  (define stored-token #f) ; To account for lookahead used for whitespace, 
					; sometimes store a token before process-indent
	  (define spaces 0) ; The amount of whitespace encountered in this line

	  (define (adjust-brace-depth token)
	    (cond [(member (token-name token) '(LP LCB LSB))
		   (set! brace-depth (+ brace-depth 1))]
		  [(member (token-name token) '(RP RCB RSB))
		   (set! brace-depth (- brace-depth 1))]))

#|
	  (define (blab t)
	    (display "At ") (display t) (newline)
	    (display "Brace-depth: ") (display brace-depth) (newline)
	    (display "State: ") (display state) (newline))
|#
	  
	  #| TODO: Probably rewrite as generator |#
	  (define (get-logical-token input-port)
	    (let ((continue (lambda () (get-logical-token input-port))))
	      (case state
		[(begin-line) 
		 (let ((physical-token (lex input-port)))
		   (case (token-name physical-token)
		     [(PHYSICAL-NEWLINE) 
		      (continue)]
		     [(WHITESPACE)
		      (set! state 'leading-ws)
		      (set! spaces (token-value physical-token))
		      (continue)]
		     [else
		      (set! state 'pre-slurg)
		      (set! stored-token physical-token)
		      (continue)]))]
		[(leading-ws) 
		 (let ((physical-token (lex input-port)))
		   (case (token-name physical-token)
		     [(PHYSICAL-NEWLINE)
		      (set! state 'begin-line)
		      (set! spaces 0)
		      (continue)]
		     [(WHITESPACE)
		      (error "Whitespace after whitespace should not occur.")]
		     [(EOF)
		      (error "Lines cannot end with EOF. (Probably.)")]
		     [else
		      (set! state 'pre-slurg)
		      (set! stored-token physical-token)
		      (continue)]))]

		[(pre-slurg) 
		 (cond
		  [(> spaces (car indent-stack))
		   (set! indent-stack (cons spaces indent-stack))
		   (token-INDENT)]
		  [(< spaces (car indent-stack))
		   (set! indent-stack (cdr indent-stack))
					; If the value of spaces is 'skipped' in indent-stack, bad dedent.
		   (if (> spaces (car indent-stack)) (error "Bad dedentation")
		       (token-DEDENT))]
		  [stored-token ; After all dedents/indents are consumed, the other token is left...
		   (let ((t stored-token))
		     (set! stored-token #f)
		     (adjust-brace-depth t)
		     t)]
		  [#t 
		   (set! state 'slurg)
		   (continue)])]
		[(slurg) 
		 (let ((physical-token (lex input-port)))
		   (case (token-name physical-token)
		     [(EOF)
		      (if (> brace-depth 0)
			  (error "File ended inside braces.")
			  (error "Lines cannot end with EOF."))]
		     [(PHYSICAL-NEWLINE)
		      (if (> brace-depth 0)
			  (continue)
			  (begin
			    (set! state 'begin-line)
			    (set! spaces 0)
			    (token-NEWLINE)))]
		     [(WHITESPACE)
		      (continue)]
		     [else
		      (adjust-brace-depth physical-token)
		      physical-token]))])))

	  )
	 get-logical-token))
