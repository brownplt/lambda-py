#lang racket

(require parser-tools/yacc
	 "python-lexer.rkt")

(provide parse-python)

#| Support load through very simple expressions to get *False running |#
(define (fake-set-ctx ctx-nodetype ast)
  (hash-set
   (if (hash-has-key? ast 'value)
       (hash-set ast 'value
		 (fake-set-ctx ctx-nodetype (hash-ref ast 'value)))
       ast)
   'ctx (hasheq 'nodetype ctx-nodetype)))

(define parse-python
  (parser
   (start file-input)
   (end EOF)
   (error (lambda (a name val)
	    (display "Parser error: " ) (display a) (display ", ")
	    (display name) (display ", ") (display val) (newline)))
   (tokens nonempty-other-tokens empty-logical-tokens empty-other-tokens)
   (precs)

   (grammar
    (file-input [(file-input-lines) (hasheq 'nodetype "Module"
					  'body (reverse $1))])
    (file-input-lines
     #;[(stmt) $1] ; TEMP
     [() (list)]
     [(file-input-lines NEWLINE) $1]
     [(file-input-lines stmt ) (append $2 $1)])

    (stmt
     [(simple-stmt) $1]
     #;[(compound-stmt) (list $1)])

    (simple-stmt
     [(stmt-list SEMICOLON NEWLINE) $1]
     [(stmt-list NEWLINE) $1])

    (stmt-list
     [(small-stmt) (list $1)]
     [(stmt-list SEMICOLON small-stmt) (cons $3 $1)])

    (small-stmt
     [(expr-stmt) $1]
     #;[(del-stmt) $1]
     #;[(pass-stmt) $1]
     #;[(flow-stmt) $1]
     #;[(import-stmt) $1]
     #;[(global-stmt) $1]
     #;[(nonlocal-stmt) $1]
     #;[(assert-stmt) $1])

     (expr-stmt
      [(testlist-star-expr) (hasheq 'nodetype "Expr"
				  'value (fake-set-ctx "Load" $1))]

      #| At this point, recur to set ctx? |#
      #;[(testlist-star-expr augassign single-expr) (hash)]
      #;[(testlist-star-expr assignment-chain) (hash)]
      
      )

     (testlist-star-expr
      [(test-or-star-expr) $1]
      [(test-or-star-expr COMMA) (hasheq 'nodetype "Tuple"
				       'elts (list $1))]
      #;[(test-or-star-list) ()]
      #;[(test-or-star-list COMMA) ()])

     (test-or-star-expr
      #;[(test) $1]
      [(* expr) (hasheq 'nodetype "Starred"
		      'value $2)])
     
     (expr
      [(xor-expr) $1]
      #;[(expr BAR xor-expr) ()])

     (xor-expr 
      [(and-expr) $1]
      #;[(xor-expr ^ and-expr) ()])
     
     (and-expr
      [(shift-expr) $1]
      #;[(and-expr & shift-expr)])

     (shift-expr
      [(arith-expr) $1]
      #;[(shift-expr << arith-expr) ()]
      #;[(shift-expr >> arith-expr) ()])

     (arith-expr
      [(term) $1]
      #;[(arith-expr + term)]
      #;[(arith-expr - term)])

     (term
      [(factor) $1]
      #;[(term * factor) ()]
      #;[(term / factor) ()]
      #;[(term % factor) ()]
      #;[(term // factor) ()])

     (factor
      [(power) $1]
      #;[(+ factor) ()]
      #;[(- factor) ()]
      #;[(~ factor) ()])
     
     (power
      [(atom) $1] ; Temp
      #;[(atom trailer-list) ()]
      #;[(atom trailer-list ** factor) ()])

#|
atom: ('(' [yield_expr|testlist_comp] ')' |
       '[' [testlist_comp] ']' |
       '{' [dictorsetmaker] '}' |
       NAME | NUMBER | STRING+ | '...' | 'None' | 'True' | 'False')
|#
     
     (atom
      [(IDENTIFIER) (hasheq 'nodetype "Name"
			  'id $1)]
      [(False) (hasheq 'nodetype "Name"
		     'id "False")]
      [(True) (hasheq 'nodetype "Name"
		     'id "True")]
      [(None) (hasheq 'nodetype "Name"
		     'id "None")]
      #;[MANY])

	

)))
    

