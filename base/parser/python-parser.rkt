#lang racket

(require racket/match
         "python-lexer.rkt"
         (prefix-in grammar: "python-grammar.rkt"))

(provide parse-python)

#| 
AST: hash of symbol -> (string | number | AST | #\nul)
Values of these will match the ASDL spec at http://docs.python.org/3.2/library/ast.html with the constructor name under the 'nodetype key.

This format matches that produced by python-python-parser.rkt and python-parser.py. The main non-obvious aspect of it is the 'ctx attribute of each expression node, which is determined by the statement type (or generator expression).
|#
(define ast hasheq)

#|
Ragg generates a parse tree in the form of a syntax object, which can look oddly nested because of Python's grammar:

Example:
(syntax->datum (parse '(NAME NEWLINE)))
'(file_input
  (stmt
   (simple_stmt
    (small_stmt
     (expr_stmt
      (testlist (test (or_test (and_test (not_test (comparison (expr (xor_expr (and_expr (shift_expr (arith_expr (term (factor (power (atom NAME)))))))))))))))))
    NEWLINE)))

Most of this nesting is handled by the first clauses of expr->ast and non-simple-stmt->ast.

Grammar LHS pseudo-types used in function names:
stmt: any expression that can be derived starting with stmt - Python's statements
expr: any expression that can be derived starting with testlist_star_expr (probably) - Python's expression language
module/trailer/comp-op/suite/decorator etc.: Name matches the grammar LHS (here, their car)

Also, I hope you like quasiquote match patterns. (Sorry.) This should move to syntax objects and syntax-case to support source position eventually.
|#

(define (parse-python port)
  (module->ast (syntax->datum (grammar:parse (get-python-lexer port)))))

(define (module->ast py-ragg)
  (match py-ragg
    [(list 'file_input stmts ...)
     (ast 'nodetype "Module"
          'body (flatten (map stmt->ast-list stmts)))]
    [_ (error "Only file_input is supported.")]))

;; simple_stmt is the only derivation of stmt that Python parses into multiple statements
(define (stmt->ast-list py-ragg)
  (match py-ragg
    [`(stmt ,any-stmt) (stmt->ast-list any-stmt)]
    [`(simple_stmt ,clauses ... "NEWLINE")
     (define (more-clauses clauses)
       (match clauses
         [`() '()]
         [`(";" ,rest ...) (more-clauses rest)]
         [`(,statement ,rest ...) (cons (non-simple-stmt->ast statement) (more-clauses rest))]))
     (more-clauses clauses)]
    [other-statement (list (non-simple-stmt->ast other-statement))]))

;; Transform derivations of stmt other than simple_stmt into ast
(define (non-simple-stmt->ast py-ragg)
  (match py-ragg
    [(list (or 'stmt 'flow_stmt 'small_stmt 'compound_stmt) stmt) 
     (non-simple-stmt->ast stmt)]

    [(list 'yield_stmt expr)
     (ast 'nodetype "Expr"
          'value (expr->ast expr "Load"))]

    [(list 'continue_stmt "continue") 
     (ast 'nodetype "Continue")]
    
    [(list 'break_stmt "break")
     (ast 'nodetype "Break")]
    
    ;; TODO: Allow only assignments to those allowed by 
    ;; http://docs.python.org/3.2/reference/simple_stmts.html#assignment-statements
    [(list 'expr_stmt testlist (list 'augassign op) val)
     (ast 'nodetype "AugAssign"
          'op (ast 'nodetype (case op 
                               [("+=") "Add"]
                               [("-=") "Sub"]
                               [("*=") "Mult"]
                               [("/=") "Div"]
                               [("%=") "Mod"]
                               [("&=") "BitAnd"]
                               [("|=") "BitOr"]
                               [("^=") "BitXor"]
                               [(">>=") "RShift"]
                               [("<<=") "LShift"]
                               [("**=") "Pow"]
                               [("//=") "FloorDiv"]
                               [else (error "Unrecognized augassign op")]))
          'target (expr->ast testlist "Store")
          'value (expr->ast val "Load"))]

    [`(expr_stmt ,clauses ...)
     (define (more-clauses clauses targets)
       (match clauses
         [`(,val) (if (null? targets)
                     (ast 'nodetype "Expr"
                          'value (expr->ast val "Load"))
                     (ast 'nodetype "Assign"
                          'value (expr->ast val "Load")
                          'targets (reverse targets)))]
         [`(,target "=" ,rest ...)
          (more-clauses rest (cons (expr->ast target "Store")
                                   targets))]))
     (more-clauses clauses '())]

    [(list 'return_stmt "return" val)
     (ast 'nodetype "Return"
          'value (expr->ast val "Load"))]

    [(list 'return_stmt "return")
     (ast 'nodetype "Return"
          'value #\nul)]

    [(list 'del_stmt "del" expr)
     (ast 'nodetype "Delete"
          'targets (exprlist->ast-list expr "Del"))]

    [(list 'raise_stmt "raise")
     (ast 'nodetype "Raise"
          'exc #\nul
          'cause #\nul)]

    [(list 'raise_stmt "raise" exc)
     (ast 'nodetype "Raise"
          'exc (expr->ast exc "Load")
          'cause #\nul)]

    [(list 'raise_stmt "raise" exc "from" inner-exc)
     (ast 'nodetype "Raise"
          'exc (expr->ast exc "Load")
          'cause (expr->ast inner-exc "Load"))]

    [(list 'pass_stmt "pass")
     (ast 'nodetype "Pass")]

    [(list 'assert_stmt "assert" expr)
     (ast 'nodetype "Assert"
          'test (expr->ast expr "Load")
          'msg #\nul)]

    [(list 'assert_stmt "assert" test-expr "," msg-expr)
     (ast 'nodetype "Assert"
          'test (expr->ast test-expr "Load")
          'msg (expr->ast msg-expr "Load"))]

    [(list 'global_stmt "global" rest ...)
     (ast 'nodetype "Global"
          'names (map cdr (every-other rest)))]

    [(list 'nonlocal_stmt "nonlocal" rest ...)
     (ast 'nodetype "Nonlocal"
          'names (map cdr (every-other rest)))]

    [`(with_stmt "with" ,clauses ... ":" ,with-suite)
     (car
      (foldl 
       (lambda (with_clause inner-ast-list)
         (match with_clause
           [`(with_item ,elt-expr)
            (list (ast 'nodetype "With"
                       'body inner-ast-list
                       'optional_vars #\nul
                       'context_expr (expr->ast elt-expr "Load")))]
           [`(with_item ,elt-expr "as" ,var-expr)
            (list (ast 'nodetype "With"
                       'body inner-ast-list
                       'optional_vars (expr->ast var-expr "Store")
                       'context_expr (expr->ast elt-expr "Load")))]
           [_ (error "Bad with clause")]))
       (suite->ast-list with-suite)
       (reverse (every-other clauses))))]

    [`(if_stmt "if" ,test ":" ,suite ,rest ...)
     (define (more-clauses clauses)
       (match clauses
         [`() '()]
         [`("elif" ,test ":" ,suite ,rest ...) 
          (list (ast 'nodetype "If"
                     'test (expr->ast test "Load")
                     'body (suite->ast-list suite)
                     'orelse (more-clauses rest)))]
          [`("else" ":" ,suite)
           (suite->ast-list suite)]))
     (ast 'nodetype "If"
          'test (expr->ast test "Load")
          'body (suite->ast-list suite)
          'orelse (more-clauses rest))]
    
    [`(import_stmt (import_name "import" (dotted_as_names ,names ...)))
     (ast 'nodetype "Import"
          'names (map (lambda (name)
                        (match name
                          [`(dotted_as_name ,name) 
                           (ast 'nodetype "alias"
                                'name (dotted-name->string name)
                                'asname #\nul)]
                          [`(dotted_as_name ,name "as" (name . ,as-name))
                           (ast 'nodetype "alias"
                                'name (dotted-name->string name)
                                'asname as-name)]))
                      (every-other names)))]

    [`(import_stmt (import_from "from" ,maybe-source-name ... "import" ,destinations ...))
     (define (fold-sources sources level name)
       (match sources
         [`() (values level name)]
         [`("." ,rest ...) (fold-sources rest (+ 1 level) name)]
         [`("..." ,rest ...) (fold-sources rest (+ 3 level) name)]
         [`(,dotted-name ,rest ...) (fold-sources rest level (dotted-name->string dotted-name))]
         [_ (display sources) (newline) (error "Unhandled import source")]))
     (define-values (level source-name) (fold-sources maybe-source-name 0 #\nul))
     (define (import-as-name->ast name)
       (match name
         [`(import_as_name (name . ,name))
          (ast 'nodetype "alias" 
               'name name
               'asname #\nul)]
         [`(import_as_name (name . ,name) "as" (name . ,interior-name))
          (ast 'nodetype "alias"
               'name name
               'asname interior-name)]))
     (ast 'nodetype "ImportFrom"
          'names (match destinations
                   [`((import_as_names ,names ...))
                    (map import-as-name->ast (every-other names))]
                   [`("(" (import_as_names ,names ...) ")")
                    (map import-as-name->ast (every-other names))]
                   [`("*") (list (ast 'nodetype "alias"
                                      'name "*"
                                      'asname #\nul))]
                   [_ (display destinations) (newline) (error "Unmatched import destination")])
          'level level
          'module source-name)]

    [`(try_stmt "try" ":" ,try-suite ,rest ...)
     (local ((define (more-clauses lst handler-ast-list orelse-ast-list finalbody-ast-list)
               (if (null? lst)
                   (let ((try-except-ast 
                          (ast 'nodetype "TryExcept"
                               'body (suite->ast-list try-suite)
                               'orelse orelse-ast-list
                               'handlers (reverse handler-ast-list))))
                     (if (null? finalbody-ast-list) 
                         try-except-ast
                         (ast 'nodetype "TryFinally"
                              'body (if (null? handler-ast-list)
                                        (suite->ast-list try-suite)
                                        (list try-except-ast))
                              'finalbody finalbody-ast-list)))
                   (match lst
                     [`("finally" ":" ,finally-suite ,rest ...)
                      (more-clauses rest
                                    handler-ast-list
                                    orelse-ast-list
                                    (suite->ast-list finally-suite))]
                     [`("else" ":" ,else-suite ,rest ...)
                      (more-clauses rest
                                    handler-ast-list
                                    (suite->ast-list else-suite)
                                    finalbody-ast-list)]
                     [`((except_clause "except" ,test-and-name ...) ":" ,handler-suite ,rest ...)
                      (define-values (test-ast error-name)
                        (match test-and-name 
                          [`(,test "as" (name . ,name)) (values (expr->ast test "Load") name)]
                          [`(,test) (values (expr->ast test "Load") #\nul)]
                          [`() (values #\nul #\nul)]))
                      (more-clauses rest
                                    (cons 
                                     (ast 'nodetype "ExceptHandler"
                                          'body (suite->ast-list handler-suite) 
                                          'name error-name
                                          'type test-ast)
                                     handler-ast-list)
                                    orelse-ast-list
                                    finalbody-ast-list)]))))
            (more-clauses rest '() '() '()))]

    [`(while_stmt "while" ,test-expr ":" ,body-suite ,maybe-else ...)
     (ast 'nodetype "While"
          'test (expr->ast test-expr "Load")
          'orelse (match maybe-else
                    [`("else" ":" ,else-suite) (suite->ast-list else-suite)]
                    [`() '()])
          'body (suite->ast-list body-suite))]

    [`(for_stmt "for" ,bound-list "in" ,expr-list ":" ,body-suite ,maybe-else ...)
     (ast 'nodetype "For"
          'iter (expr->ast expr-list "Load")
          'target (exprlist->ast bound-list "Store")
          'body (suite->ast-list body-suite)
          'orelse (match maybe-else
                    [`("else" ":" ,else-suite) (suite->ast-list else-suite)]
                    [`() '()]))]

    ;; funcdef TODO: Unfinished formal handling in build-formals
    [`(funcdef "def" 
               (name . ,name) 
               ,parameters ":" ,suite)
     (ast 'nodetype "FunctionDef"
          'body (suite->ast-list suite)
          'args (build-formals (parameters->arg-sexp parameters))
          'name name
          'returns #\nul
          'decorator_list '())]

    [`(decorated
       (decorators 
        ,decorators ...)
       (funcdef "def"
                (name . ,func-name)
                ,parameters ":" ,suite))
     (ast 'nodetype "FunctionDef"
          'body (suite->ast-list suite)
          'args (build-formals (parameters->arg-sexp parameters))
          'name func-name
          'returns #\nul
          'decorator_list 
          (map decorator->ast decorators))]

    [`(classdef "class" (name . ,name) ,maybe-args ... ":" ,suite)
     (build-call (match maybe-args
                   [`("(" (arglist ,args ...) ")") args]
                   [`("(" ")") '()]
                   [`() '()])
                 (lambda (posargs keywords kwarg stararg)
                              (ast 'nodetype "ClassDef"
                                   'body (suite->ast-list suite)
                                   'bases posargs
                                   'name name
                                   'decorator_list '()
                                   'kwargs kwarg
                                   'starargs stararg
                                   'keywords keywords)))]

    [`(decorated
       (decorators 
        ,decorators ...)
       (classdef "class" (name . ,name) ,maybe-args ... ":" ,suite))
     (build-call (match maybe-args
                   [`("(" (arglist ,args ...) ")") args]
                   [`("(" ")") '()]
                   [`() '()])
                 (lambda (posargs keywords kwarg stararg)
                              (ast 'nodetype "ClassDef"
                                   'body (suite->ast-list suite)
                                   'bases posargs
                                   'name name
                                   'decorator_list (map decorator->ast decorators)
                                   'kwargs kwarg
                                   'starargs stararg
                                   'keywords keywords)))]

    [_ 
     (display "=== Unhandled grammar ===\n")
     (pretty-write py-ragg)
     (error (string-append "Unhandled grammar"))]))

(define (parameters->arg-sexp parameters)
  (match parameters
    [`(parameters "(" (typedargslist ,args ...) ")") args]
    [`(parameters "(" ")") '()]))

;; Destructure ragg python expressions to python ast with ctx value appropriate to the statement, expression, and position.
;; I'm assuming for now that this will handle testlist *and* test, in all cases.
;; I believe the expr-ctx passed on will inherently be "Load" in almost all cases, 
;; but I'm passing expr-ctx forward until I'm sure.
(define (expr->ast py-ragg expr-ctx)
  (match py-ragg

    #| Expression fallthroughs... |#
    [(list (or 'test_nocond 'argument 'testlist_comp 'testlist_star_expr 'testlist 'test 'or_test 'and_test 'not_test 
               'comparison 'expr 'xor_expr 'and_expr 'shift_expr 'arith_expr 'term 'factor 'power) 
           expr)
     (expr->ast expr expr-ctx)]

    ;; Single item handled above. All others are tuples.
    ;; Unhandled star_expr will be caught downwind.
    [(list (or 'testlist_star_expr 'testlist) elements ...)
     (ast 'nodetype "Tuple"
          'ctx (ast 'nodetype expr-ctx)
          'elts (map (lambda (e) (expr->ast e expr-ctx)) (every-other elements)))]
    
    [`(star_expr "*" ,val)
     (if (equal? expr-ctx "Store")
         (ast 'nodetype "Starred"
              'ctx (ast 'nodetype expr-ctx)
              'value (expr->ast val expr-ctx))
         (error "Starred expressions can only be used in assignment"))]

    [(list 'test t-expr "if" cond-expr "else" f-expr)
     (ast 'nodetype "IfExp"
          'test (expr->ast cond-expr "Load")
          'body (expr->ast t-expr "Load")
          'orelse (expr->ast f-expr "Load"))]

    [`(,(or 'lambdef 'lambdef_nocond) "lambda" ,maybe-args ... ":" ,expr)
     (ast 'nodetype "Lambda"
          'args (build-formals (match maybe-args
                                 [`((varargslist ,args ...)) args]
                                 [`() '()]))
          'body (expr->ast expr "Load"))]

    [(list 'yield_expr "yield" expr)
     (ast 'nodetype "Yield"
          'value (expr->ast expr "Load"))]

    [(list 'yield_expr "yield")
     (ast 'nodetype "Yield"
          'value #\nul)]
    
    ;; Single item is caught above.
    [(list 'comparison expr1 rest ...)
     (let ((ops (every-other rest))
           (exprs (every-other (cdr rest))))
       (ast 'nodetype "Compare"
            'left (expr->ast expr1 expr-ctx)
            'ops (map comp-op->ast ops)
            'comparators (map (lambda (e) (expr->ast e expr-ctx)) exprs)))]

    [(list (or 'term 'arith_expr 'expr 'xor_expr 'and_expr 'shift_expr) expr1 rest ...)
     (let ((ops (every-other rest))
           (exprs (if (null? rest) '() (every-other (cdr rest)))))
       (foldl (lambda (op right-expr left-ast)
                (ast 'nodetype "BinOp"
                     'left left-ast
                     'right (expr->ast right-expr expr-ctx)
                     'op (ast 'nodetype (case op 
                                          [("+") "Add"] 
                                          [("-") "Sub"]
                                          [("/") "Div"]
                                          [("*") "Mult"]
                                          [("%") "Mod"]
                                          [("//") "FloorDiv"]
                                          [("|") "BitOr"]
                                          [("^") "BitXor"]
                                          [("&") "BitAnd"]
                                          [("<<") "LShift"]
                                          [(">>") "RShift"]
                                          [else (error "Bad arith/term op")]))))
              (expr->ast expr1 expr-ctx)
              ops
              exprs))]

    ;; Single item right hand sides should be caught in the fallthrough clause above.
    [(list (or 'and_test 'or_test) rest ...)
     (let ((exprs (every-other rest)))
       (ast 'nodetype "BoolOp"
            'op (ast 'nodetype (case (second rest)
                                 [("or") "Or"] 
                                 [("and") "And"] 
                                 [else (error "Bad boolean op")]))
            'values (map (lambda (e) (expr->ast e expr-ctx)) exprs)))]

    ;; Set expr-ctx as ctx on last of trailers... Pass "Load" to interiors
    [(list 'power val all-trailers ... "**" power)
       (ast 'nodetype "BinOp"
            'op (ast 'nodetype "Pow")
            'left (if (null? all-trailers) (expr->ast val "Load")
                      (let* ((rev-trailers (reverse all-trailers))
                             (trailers (reverse (rest rev-trailers)))
                             (last-trailer (first rev-trailers)))
                      (wrap-with-trailer 
                       last-trailer 
                       expr-ctx
                       (foldl 
                        (lambda (trailer left-ast)
                          (wrap-with-trailer trailer "Load" left-ast))
                        (expr->ast val "Load")
                        trailers))))
            'right (expr->ast power "Load"))]
    
    [(and (list 'power val trailers ... last-trailer)
          (not (list _ ... "**" _)))
     (wrap-with-trailer 
      last-trailer 
      expr-ctx
      (foldl 
       (lambda (trailer left-ast)
         (wrap-with-trailer trailer "Load" left-ast))
       (expr->ast val "Load")
       trailers))]

    [(list 'not_test "not" expr)
     (ast 'nodetype "UnaryOp"
          'op (ast 'nodetype "Not")
          'operand (expr->ast expr expr-ctx))]
    
    [(list 'factor op expr)
     (ast 'nodetype "UnaryOp"
          'op (ast 'nodetype (case op [("+") "UAdd"] [("-") "USub"] [("~") "Invert"]))
          'operand (expr->ast expr expr-ctx))]

    ;; Cons here is from token construction in the lexer
    [(list 'atom (cons 'name name))
     (ast 'nodetype "Name"
          'id name
          'ctx (ast 'nodetype expr-ctx))]

    [(list 'atom (cons 'string str-parts) ...)
     (ast 'nodetype "Str"
          's (apply string-append str-parts))]

    ;; str-parts do contain escape sequences here. Might be an artifact of the original non-bytes assignment.
    [(list 'atom (cons 'bytes str-parts) ...)
     (ast 'nodetype "Bytes"
          's (ast 'nodetype "Bytes"
                      'value (string-append "b'" (apply string-append str-parts) "'")))]

    ;; Bytes sequences and string sequences have been matched already...
    [(or (list 'atom (cons 'string str-parts) rest ...)
         (list 'atom (cons 'bytes str-parts) rest ...))
     (error "Cannot mix string and bytestring literals")]

    ;; atom TODO: '...' (in lexer)
    ;; Note: True, False, None lexed as names though they're in the grammar used.
    [(list 'atom (cons type val))
     (if (equal? expr-ctx "Store")
         (error "Cannot store to a literal")
         (case type
           [(integer float) (ast 'nodetype "Num" 'n val)]
           [(imaginary) (ast 'nodetype "Num" 
                             'n (ast 'nodetype "Complex" 'value val))]
           [else (error "Literal not handled yet")]))]

    #| atom curly brace forms |#
    [`(atom "{" "}")
     (ast 'nodetype "Dict"
          'values '()
          'keys '())]

    ;; Set comprehension
    [`(atom "{" (dictorsetmaker ,val-expr ,(and comp (list 'comp_for _ ...))) "}")
     (build-comprehension comp (lambda (generators)
                                 (ast 'nodetype "SetComp"
                                      'generators generators
                                      'elt (expr->ast val-expr "Load"))))]

    ;; Dict comprehension
    [`(atom "{" (dictorsetmaker ,key-expr ":" ,val-expr ,(and comp `(comp_for ,_ ...))) "}")
     (build-comprehension comp (lambda (generators)
                                 (ast 'nodetype "DictComp"
                                      'generators generators
                                      'key (expr->ast key-expr "Load")
                                      'value (expr->ast val-expr "Load"))))]
    
    ;; Dict literal
    [`(atom "{" (dictorsetmaker ,key ":" ,rest ...) "}")
     (local ((define (more-items items key-exprs value-exprs)
               (match items
                 [`() 
                  (ast 'nodetype "Dict"
                       'keys (map expr->value-ast (reverse key-exprs))
                       'values (map expr->value-ast (reverse value-exprs)))]
                 [`("," ,more ...)
                  (more-items more key-exprs value-exprs)]
                 [`(,key-expr ":" ,value-expr ,more ...)
                  (more-items more (cons key-expr key-exprs) (cons value-expr value-exprs))])))
            (more-items (cons key (cons ":" rest)) '() '()))]

    ;; Set literal
    [`(atom "{" (dictorsetmaker ,rest ...) "}")
     (ast 'nodetype "Set"
          'elts (map expr->value-ast (every-other rest)))]

    #| atom square bracket forms |#
    [`(atom "[" "]")
     (ast 'nodetype "List"
          'elts '()
          'ctx (ast 'nodetype "Load"))]

    ;; List comprehension
    [`(atom "[" (testlist_comp ,elt-expr ,(and comp (list 'comp_for _ ...))) "]")
     (build-comprehension comp
                          (lambda (generators)
                            (ast 'nodetype "ListComp"
                                 'elt (expr->ast elt-expr "Load")
                                 'generators generators)))]

    ;; List literal
    [`(atom "[" (testlist_comp ,exprs ...) "]")
     (ast 'nodetype "List"
          'ctx (ast 'nodetype "Load")
          'elts (map expr->value-ast (every-other exprs)))]

    #| atom paren forms |#
    ;; 0-tuple
    [`(atom "(" ")")
     (ast 'nodetype "Tuple"
          'ctx (ast 'nodetype expr-ctx)
          'elts '())]

    ;; Generator 
    [`(atom "(" (testlist_comp ,elt-expr ,(and comp (list 'comp_for _ ...))) ")")
     (build-comprehension comp (lambda (generators)
                                 (ast 'nodetype "GeneratorExp"
                                      'elt (expr->ast elt-expr "Load")
                                      'generators generators)))]

    ;; Tuple
    [`(atom "(" (testlist_comp ,expr1 "," ,exprs ...) ")")
     (ast 'nodetype "Tuple"
          'ctx (ast 'nodetype expr-ctx)
          'elts (map (lambda (e) (expr->ast e expr-ctx)) (cons expr1 (every-other exprs))))]

    ;; Parenthesized expression
    [`(atom "(" ,expr ")")
     (expr->ast expr expr-ctx)]

    [_ 
     (display "=== Unhandled expression ===\n")
     (pretty-write py-ragg)
     (error (string-append "Unhandled expression"))]))

;; A "Load" position convenience, esp for map
(define (expr->value-ast expr)
  (expr->ast expr "Load"))

;; Turn `(dotted_name ...) rep into a.b string
(define (dotted-name->string name)
  (match name
    [`(dotted_name (name . ,init-name) ,segments ...)
     (foldl (lambda (name-segment name)
              (match name-segment
                [`(name . ,id-part) (string-append name id-part)]
                ["." (string-append name ".")]))
            init-name
            segments)]))

;; Turn dotted_name rep into expr ast  
(define (dotted-name->ast name)
  (match name
    [`(dotted_name (name . ,init-name) ,segments ...)
     (foldl (lambda (name-segment name-ast)
              (match name-segment
                [`(name . ,id-part) 
                 (ast 'nodetype "Attribute"
                      'attr id-part
                      'ctx (ast 'nodetype "Load")
                      'value name-ast)]
                ["." name-ast]))
            (ast 'nodetype "Name"
                 'ctx (ast 'nodetype "Load")
                 'id init-name)
            segments)]))

(define (comp-op->ast comp-op)
  (ast 'nodetype
       (match (cdr comp-op)
         [`("<") "Lt"]
         [`(">") "Gt"]
         [`("==") "Eq"]
         [`(">=") "GtE"]
         [`("<=") "LtE"]
         [`("!=") "NotEq"]
         [`("in") "In"]
         [`("not" "in") "NotIn"]
         [`("<>") (error "<> operator is not supported.")]
         [`("is") "Is"]
         [`("is" "not") "IsNot"])))

(define (suite->ast-list suite)
  (match suite
    [(list 'suite stmt) 
     (flatten (stmt->ast-list stmt))]
    [(list 'suite "NEWLINE" "INDENT" stmts ... "DEDENT")
     (flatten (map stmt->ast-list stmts))]))

(define (wrap-with-trailer trailer expr-ctx left-ast)
  (local ((define (make-call-ast args keywords kwarg stararg)
            (ast 'nodetype "Call"
                 'args args
                 'kwargs kwarg
                 'starargs stararg
                 'keywords keywords
                 'func left-ast))
          (define (attr-ast attr)
            (ast 'nodetype "Attribute"
                 'ctx (ast 'nodetype expr-ctx)
                 'attr attr
                 'value left-ast))
          (define (subscript->ast s slice?)
            (define (maybe-test->ast t) ;; (test ...) or '()
              (if (null? t) #\nul
                  (expr->value-ast (first t))))
            (match s
              ;; The grammar here is [test] : [test] [sliceop]
              [`(subscript ,maybe-lower ... ":"
                           ,(and maybe-upper `(test ,_)) ...
                           (sliceop ":" ,maybe-step) ...
                           (sliceop ":") ...)
               (ast 'nodetype "Slice"
                    'lower (maybe-test->ast maybe-lower)
                    'upper (maybe-test->ast maybe-upper)
                    'step (maybe-test->ast maybe-step))]
              [`(subscript ,index) 
               (if slice? 
                   (ast 'nodetype "Index"
                        'value (expr->value-ast index))
                   (expr->value-ast index))]
              [_ (error "Unhandled subscript")])))
                                
    (match trailer
      [`(trailer "(" ")") (build-call '() make-call-ast)]
      [`(trailer "(" (arglist ,args ...) ")") (build-call args make-call-ast)]
      [`(trailer "." (name . ,name)) (attr-ast name)]
      
      ;; Subscript lists: "Subscript" containing slice of:
      ;; 2+ subscripts, at least one slice -> "ExtSlice" w/dims as "Slice" and "Index"
      ;; 2+ subscripts, no slices -> "Index" w/value "Tuple" w/elts as values
      ;; 1 subscript, slice -> "Slice" 
      ;; 1 subscript, not slice -> "Index" w/value as value
      ;; This is probably more hairy than necessary
      [`(trailer "[" (subscriptlist ,subscripts ...) "]")
       (ast 'nodetype "Subscript"
            'ctx (ast 'nodetype expr-ctx)
            'value left-ast
            'slice (let* ((multiple-subscripts? (> (length subscripts) 1))
                          (slice? (findf (match-lambda [`(subscript ,_ ... ":" ,_ ...) #t]
                                                       [else #f]) subscripts))
                          (subscript-asts (map (lambda (s) (subscript->ast s slice?)) (every-other subscripts))))
                     (cond [(and multiple-subscripts? slice?)
                            (ast 'nodetype "ExtSlice"
                                 'dims subscript-asts)]
                           [multiple-subscripts?
                            (ast 'nodetype "Index"
                                 'value (ast 'nodetype "Tuple"
                                             'ctx (ast 'nodetype "Load")
                                             'elts subscript-asts))]
                           [slice? (first subscript-asts)]
                           [else (ast 'nodetype "Index"
                                      'value (first subscript-asts))])))]
           
           [_ 
            (display "=== Unhandled trailer ===\n")
            (pretty-write trailer)
            (error "Unsupported trailer (arglist) shape")])))

(define (decorator->ast decorator)
  (match decorator 
    [`(decorator "@" ,dec-name ,decorator-rest ...)
     (let ((dec-ast (dotted-name->ast dec-name)))
       (match decorator-rest
         [`("NEWLINE") dec-ast]
         [`("(" ")" "NEWLINE") (ast 'nodetype "Call"
                                    'args '()
                                    'keywords '()
                                    'kwargs #\nul
                                    'starargs #\nul
                                    'func dec-ast)]
         [`("(" (arglist ,args ...) ")" "NEWLINE")
          (build-call args (lambda (args keywords kwarg stararg)
                             (ast 'nodetype "Call"
                                  'args args
                                  'keywords keywords
                                  'kwargs kwarg
                                  'starargs stararg
                                  'func dec-ast)))]
         [_ (error "Unhandled decorator rest")]))]
    [_ (error "Unhandled decorator")]))

(define (every-other lst)
  (cond [(null? lst) '()]
        [(null? (cdr lst)) (list (car lst))]
        [else (cons (car lst) (every-other (cddr lst)))]))

;; Process arglist and call make-call-ast with args, keywords, kwarg and stararg
(define (build-call args make-call-ast)
  (local ((define (more-args args pos-args key-args stararg kwarg)
            (match args
              [`() (make-call-ast (reverse pos-args) (reverse key-args) kwarg stararg)]
              [`("," ,more ...)
               (more-args more pos-args key-args stararg kwarg)]
              [(list (and arg-parts (not ",")) ... more ...)
               (match arg-parts
                 [`("*" ,(and (not ",") vararg-expr))
                  (more-args more pos-args key-args (expr->value-ast vararg-expr) kwarg)]
                 [`("**" ,kwarg-expr)
                  (more-args more pos-args key-args stararg (expr->value-ast kwarg-expr))]
                 [`((argument ,key "=" ,val))
                  (more-args more pos-args 
                             (cons (ast 'nodetype "keyword"
                                        ;; This is backwards but works - See the note in the grammar for argument.
                                        'arg (hash-ref (expr->value-ast key) 'id)
                                        'value (expr->value-ast val)) key-args)
                             stararg kwarg)]
                 ;; bare generators
                 [`((argument ,expr ,comp_for))
                  (more-args more 
                             (cons (build-comprehension comp_for
                                                        (lambda (generators)
                                                          (ast 'nodetype "GeneratorExp"
                                                               'elt (expr->ast expr "Load")
                                                               'generators generators)))
                                   pos-args)
                             key-args stararg kwarg)]
                 [`((argument ,expr))
                  (more-args more (cons (expr->value-ast expr) pos-args) key-args stararg kwarg)]
                 [_ 
                  (display args) (newline)
                  (error "Error parsing args")])])))
    (more-args args '() '() #\nul #\nul)))

(define (exprlist->ast lst expr-ctx)
  (match lst
    [(list 'exprlist expr) (expr->ast expr expr-ctx)]
    [(list 'exprlist rest ...)
     (ast 'nodetype "Tuple"
          'ctx (ast 'nodetype expr-ctx)
          'elts (map (lambda (e) (expr->ast e expr-ctx)) (every-other rest)))]))

(define (exprlist->ast-list lst expr-ctx)
  (map (lambda (e) (expr->ast e expr-ctx)) (every-other (cdr lst))))

;; Currently covers both typedargslist and varargslist
(define (build-formals args)
  (local (;; This needs either a macro or better taste.
          ;; A bunch of accumulators masquerading as a data structure
          ;; TODO: More accurate names for these
          (struct Formals (args default-asts vararg-name kwarg-name kwargs kw-default-asts))
          (define (arg-name->ast n)
            (ast 'nodetype "arg"
                 'annotation #\nul
                 'arg n))
          (define (formals->ast formals)
            (match-let (((struct Formals (args default-asts vararg-name 
                                                           kwarg-name kwargs kw-default-asts)) 
                         formals))
                       (ast 'args (map arg-name->ast (reverse args))
                            'defaults (reverse default-asts) ;; Defaults for only optional args
                            'nodetype "arguments"
                            'vararg vararg-name
                            'kwargannotation #\nul
                            'kwarg kwarg-name 
                            'varargannotation #\nul
                            'kw_defaults (reverse kw-default-asts) ;; A default for each kwarg (#\nul if none)
                            'kwonlyargs (map arg-name->ast (reverse kwargs)))))
          (define (more-args args kw? formals)
            (match args
              [`() 
               (if (and kw? 
                        (null? (Formals-kwargs formals)) 
                        (equal? #\nul (Formals-vararg-name formals)))
                   ;; A somewhat late and dirty way to catch this
                   (error "'*' must be followed by at least one named argument")
                   (formals->ast formals))]
              [`("," ,rest ...) (more-args rest kw? formals)]
              [(list (and arg-parts (not ",")) ... rest ...)
               (match arg-parts
                 ;; Each ,_ could be ,(or 'tfpdef 'vfpdef) for more precision
                 [`("*" (,_ (name . ,name)))
                  (more-args rest #t (struct-copy Formals formals [vararg-name name]))]
                 [`("*")
                  (more-args rest #t formals)]
                 [`("**" (,_ (name . ,name)))
                  (more-args rest kw? (struct-copy Formals formals [kwarg-name name]))]
                 [`((,_ (name . ,arg-name)) "=" ,default-expr)
                  (more-args rest kw?
                             (if kw?
                                 (struct-copy Formals formals 
                                              [kwargs (cons arg-name (Formals-kwargs formals))]
                                              [kw-default-asts
                                                (cons (expr->ast default-expr "Load") 
                                                      (Formals-kw-default-asts formals))])
                                 (struct-copy Formals formals 
                                              [args (cons arg-name (Formals-args formals))]
                                              [default-asts
                                                (cons (expr->ast default-expr "Load") 
                                                      (Formals-default-asts formals))])))]
                 [`((,_ (name . ,arg-name)))
                  (more-args rest kw?
                             (if kw?
                                 (struct-copy Formals formals 
                                              [kwargs (cons arg-name (Formals-kwargs formals))]
                                              [kw-default-asts 
                                               (cons #\nul (Formals-kw-default-asts formals))])
                                 (struct-copy Formals formals 
                                              [args (cons arg-name (Formals-args formals))])))]
                 [_ (display "=== Unhandled formal argument ===") (newline)
                    (pretty-write args)
                    (error "Unhandled formal argument")])])))
         (more-args args #f (Formals '() '() #\nul #\nul '() '()))))

(define (build-comprehension comp make-ast)
  (local ((define (generator-ast target-expr iter-expr if-exprs)
            (ast 'nodetype "comprehension"
                 'target (exprlist->ast target-expr "Store")
                 'iter (expr->ast iter-expr "Load")
                 'ifs (map expr->value-ast if-exprs)))
          (define (more-clauses comp generators cur-target cur-iter current-ifs)
            (match comp
              [`() 
               (let ((gen (generator-ast cur-target cur-iter (reverse current-ifs))))
                 (make-ast (reverse (cons gen generators))))]
              [`((comp_iter (comp_for "for" ,target-expr "in" ,iter-expr ,more ...)))
               (let ((gen (generator-ast cur-target cur-iter (reverse current-ifs))))
                 (more-clauses more (cons gen generators) target-expr iter-expr '()))]
              [`((comp_iter (comp_if "if" ,if-expr ,more ...)))
               (more-clauses more generators cur-target cur-iter (cons if-expr current-ifs))]
              [_
               (display "=== Unhandled comprehension clause ===") (newline)
               (pretty-write comp)
               (error "Unhandled comprehension clause")])))
         (match comp 
           [`(comp_for "for" ,target-expr "in" ,iter-expr ,more ...)
            (more-clauses more '() target-expr iter-expr '())]
           [_ (error "Argument to build-comprehension must be a comp_for")])))

