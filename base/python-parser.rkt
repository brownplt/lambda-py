#lang racket

(require parser-tools/yacc
	 "python-lexer.rkt")

(provide parse-python)

(define parse-python
  (parser
   (start file-input)
   (end EOF)
   (error (lambda (a name val)
	    (display "Parser error: " ) (display a) (display ", ")
	    (display name) (display ", ") (display val) (newline)))
   (tokens nonempty-other-tokens empty-logical-tokens empty-other-tokens)
   (precs)

;   (suppress) 

   (grammar
    (file-input 
     [(IDENTIFIER = INTEGER NEWLINE) ; Test main with a = 1

      #hasheq((body
	       .
	       (#hasheq((targets
			 .
			 (#hasheq((id . "a")
				  (ctx . #hasheq((nodetype . "Store")))
				  (nodetype . "Name"))))
			(value . #hasheq((n . 1) (nodetype . "Num")))
			(nodetype . "Assign"))))
	      (nodetype . "Module"))
      
      ]))))
