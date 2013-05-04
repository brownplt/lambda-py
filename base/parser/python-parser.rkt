#lang racket

(provide parse-python parser-src-loc)

(require racket/match
         "python-lexer.rkt"
         "stx-util.rkt"
         (prefix-in grammar: "python-grammar.rkt"))

(define (parse-python port)
  (destructure-parse (grammar:parse (get-python-lexer port))))

(define parser-src-loc (make-parameter #f))

#| 
AST: hash of symbol -> (string | number | AST | list of AST | #\nul)
Values of these will match the ASDL spec at http://docs.python.org/3.2/library/ast.html with the constructor name under the 'nodetype key. The exception is the "SrcLoc" nodetype which is used to wrap ASTs for which we have a specific source location (should include statements and all expressions).

This format matches that produced by python-python-parser.rkt and python-parser.py. The main non-obvious aspect of it is the 'ctx attribute of each expression node, which is determined by the statement type (or by being the target of a generator).

Sexp: In this file, sexp refers to the syntax->datum conversion of the parse tree output from Ragg. Ragg generates a parse tree in the form of a syntax object, which can look oddly nested because of Python's grammar. Inspect (syntax->datum (grammar:parse '(NAME NEWLINE))) for instance. Most of this nesting is handled by the first clauses of expr->ast and non-simple-stmt->ast.

Grammar LHS pseudo-types used in function names:
stmt: any expression that can be derived starting with stmt - Python's statements
expr: any expression that can be derived starting with testlist_star_expr (probably) - Python's expression language
module/trailer/comp-op/suite/decorator etc.: Name matches the grammar LHS (here, their car)

Source Location: There's no datatype for this. Instead, it is converted from syntax objects, looked up by ragg sexps using (get-stx), to SrcLoc AST nodes wrapping AST nodes within that sourc elocation "span". The abbreviations srcpos, src-pos, srcloc and src-loc have all been used.
|#

(define ast hasheq) 

#|
SOURCE LOCATION HANDLING

destructure-parse, ast-of-sexp, with-src-sexp and error handling use the get-stx parameter to avoid passing the syntax hash around explicitly *a lot*, since it won't change or do anything interesting during a parse.
|#
(define get-stx (make-parameter (lambda (s) (error "No syntax object retrieval defined"))))

;; An AST node with source location info of sexp (via current get-stx)
;; If #:end is also provided, the SrcLoc span reflects the "span" from sexp to end, regardless of nesting.
;; If not (parser-src-loc), the AST is returned bare for testing against Python's parser.
(define (ast-of-sexp sexp #:end [end #f] . rest)
  (let ((stx ((get-stx) sexp))
        (end-stx (if end ((get-stx) end) #f)))
    (if (parser-src-loc)
        (ast 'nodetype "SrcLoc"
             'line (syntax-line stx)
             'column (syntax-column stx)
             'position (syntax-position stx)
             'span (if end
                       (+ (- (syntax-position end-stx) (syntax-position stx)) 
                          (syntax-span end-stx))
                       (syntax-span stx))
             'ast (apply ast rest))
        (apply ast rest))))

;; If source locations are turned on, wrap AST ast in a SrcLoc AST using ragg sexp src-sexp
(define (with-src-sexp src-sexp concrete-ast #:end [end #f])
  (if (parser-src-loc)
      (let ((stx ((get-stx) src-sexp))
            (end-stx (if end ((get-stx) end) #f)))
        (ast 'nodetype "SrcLoc"
             'line (syntax-line stx)
             'column (syntax-column stx)
             'position (syntax-position stx)
             'span (if end
                       (+ (- (syntax-position end-stx) (syntax-position stx)) 
                          (syntax-span end-stx))
                       (syntax-span stx))
             'ast concrete-ast))
      concrete-ast))

#| ERROR HANDLING |#

;; Raise an exn:fail with an error message consisting of a source position
;; from stx, a newline, and the attached message.
;; If marked interal, indicate an internal error rather than a user syntax error.
(define (error-at-syntax stx msg [internal #f])
  ;; TODO: Make argument a sexp for consistency and to get (get-stx) out of everywhere.
  ;; (Alternately, change above functions to take syntaxes.)
  (error (format 
          "~a: Line ~a, Column ~a, Position ~a~n~a" 
          (if internal "Internal parsing error" "Syntax error")
          (syntax-line stx) 
          (syntax-column stx) 
          (syntax-position stx) 
          msg)))

#| Parse tree parser begins here |#

;; Call into the *->ast chain to turn ragg sexp syntax object into AST
(define (destructure-parse stx)
  ;; syntax->datum+stx-hash provides get-stx, which is an eq? hash from the ragg sexp
  ;; to their original syntax objects.
  (let-values (((datum src-hash) (syntax->datum+stx-hash stx)))
    (parameterize [(get-stx (lambda (s) (hash-ref src-hash s)))]
      (file-input->ast datum))))

(define (file-input->ast py-ragg)
  (match py-ragg
    [(list 'file_input stmts ...)
     (ast-of-sexp py-ragg
                  'nodetype "Module"
                  'body (flatten (map stmt->ast-list stmts)))]
    [_ (error-at-syntax ((get-stx) py-ragg) "Only file_input is supported." #t)]))

;; simple_stmt is the only derivation of stmt that Python parses into multiple statements
;; simple_stmt->ast-list + non-simple-stmt->ast cover the whole statement "language"
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
  (with-src-sexp py-ragg (non-simple-stmt->concrete-ast py-ragg)))

(define (non-simple-stmt->concrete-ast py-ragg)
  (match py-ragg
    [(list (or 'stmt 'flow_stmt 'small_stmt 'compound_stmt) stmt) 
     (non-simple-stmt->concrete-ast stmt)] ;; concrete here

    [(list 'yield_stmt expr)
     (ast 'nodetype "Expr"
          'value (expr->ast expr "Load"))]

    [(list 'continue_stmt "continue") 
     (ast 'nodetype "Continue")]
    
    [(list 'break_stmt "break")
     (ast 'nodetype "Break")]
    
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
                               [else (error-at-syntax ((get-stx) op) (format "Unrecognized augassign op \"~a\"" op) #t)]))
          'target (let ((t (expr->ast testlist "Store")))
                    (validate-assign-target t #f t)
                    t)
          'value (expr->ast val "Load"))]

    [`(expr_stmt ,clauses ...)
     (define (more-clauses clauses targets)
       (match clauses
         [`(,val) (if (null? targets)
                      (ast-of-sexp py-ragg
                                   'nodetype "Expr"
                                   'value (expr->ast val "Load"))
                      (begin
                        (map (lambda (t) (validate-assign-target t #f t)) targets)
                        (ast
                         'nodetype "Assign"
                         'value (expr->ast val "Load")
                         'targets (reverse targets))))]
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
          'targets (let ((asts (exprlist->ast-list expr "Del")))
                     (validate-target-list asts)
                     asts))]

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
            (list (ast-of-sexp py-ragg ;; TODO: More fine-grained placement of these clauses...
                               'nodetype "With"
                               'body inner-ast-list
                               'optional_vars #\nul
                               'context_expr (expr->ast elt-expr "Load")))]
           [`(with_item ,elt-expr "as" ,var-expr)
            (list (ast-of-sexp py-ragg ;; TODO: ^
                               'nodetype "With"
                               'body inner-ast-list
                               'optional_vars (expr->ast var-expr "Store")
                               'context_expr (expr->ast elt-expr "Load")))]
           [_ (error-at-syntax ((get-stx) with_clause) "Bad with clause" #t)]))
       (suite->ast-list with-suite)
       (reverse (every-other clauses))))]

    [`(if_stmt "if" ,test ":" ,suite ,rest ...)
     (define (more-clauses clauses)
       (match clauses
         [`() '()]
         [`("elif" ,test ":" ,suite ,rest ...) 
          (list (ast-of-sexp (car clauses) 
                             #:end py-ragg ;; Span from "elif" to end of entire chain including else
                             'nodetype "If"
                             'test (expr->ast test "Load")
                             'body (suite->ast-list suite)
                             'orelse (more-clauses rest)))]
         [`("else" ":" ,suite) ;; TODO: An optional "start" sexp or start location for suites?
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
                           (ast-of-sexp name
                                        'nodetype "alias"
                                        'name (dotted-name->string name)
                                        'asname #\nul)]
                          [`(dotted_as_name ,name "as" (name . ,as-name))
                           (ast-of-sexp name
                                        'nodetype "alias"
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
         [_ (error-at-syntax ((get-stx) (first sources)) "Error parsing import source" #t)]))
     (define (import-as-name->ast import-as-name)
       (match import-as-name
         [`(import_as_name (name . ,name))
          (ast-of-sexp import-as-name
                       'nodetype "alias" 
                       'name name
                       'asname #\nul)]
         [`(import_as_name (name . ,name) "as" (name . ,interior-name))
          (ast-of-sexp import-as-name
                       'nodetype "alias"
                       'name name
                       'asname interior-name)]))
     (define-values (level source-name) (fold-sources maybe-source-name 0 #\nul))
     (ast 'nodetype "ImportFrom"
          'names (match destinations
                   [`((import_as_names ,names ...))
                    (map import-as-name->ast (every-other names))]
                   [`("(" (import_as_names ,names ...) ")")
                    (map import-as-name->ast (every-other names))]
                   [`("*") (list (ast ;; no srcloc for now
                                  'nodetype "alias"
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
                                     (ast-of-sexp (car lst) #:end handler-suite
                                                  'nodetype "ExceptHandler"
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

    [`(funcdef "def" 
               (name . ,name) 
               ,parameters ,maybe-annotation ... ":" ,suite)
     (ast 'nodetype "FunctionDef"
          'body (suite->ast-list suite)
          'args (build-formals 
                 py-ragg #f ;; straight to ast-of-sexp...
                 (parameters->arg-sexp parameters))
          'name name
          'returns (match maybe-annotation
                     [`() #\nul]
                     [`("->" ,anno) (expr->value-ast anno)])  
          'decorator_list '())]

    [`(decorated
       (decorators 
        ,decorators ...)
       (funcdef "def"
                (name . ,func-name)
                ,parameters ":" ,suite))
     (ast 'nodetype "FunctionDef"
          'body (suite->ast-list suite)
          'args (build-formals 
                 py-ragg #f ;; straight to ast-of-sexp...
                 (parameters->arg-sexp parameters))
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

;; Destructure ragg python expressions to python ast with ctx value appropriate to the statement, expression, and position
;; I'm assuming for now that this will handle testlist *and* test, in all cases.
;; I believe the expr-ctx passed on will inherently be "Load" in almost all cases, 
;; but I'm passing expr-ctx forward until I'm sure.

(define (expr->ast py-ragg expr-ctx)
  (with-src-sexp py-ragg (expr->concrete-ast py-ragg expr-ctx)))

(define (expr->concrete-ast py-ragg expr-ctx)
  (match py-ragg

    #| Expression fallthroughs... |#
    [(list (or 'test_nocond 'argument 'testlist_comp 'testlist_star_expr 'testlist 'test 'or_test 'and_test 'not_test 
               'comparison 'expr 'xor_expr 'and_expr 'shift_expr 'arith_expr 'term 'factor 'power) 
           expr)
     (expr->concrete-ast expr expr-ctx)] ;; Note: expr->concrete-ast here.

    ;; Single item handled above. All others are tuples.
    ;; Unhandled star_expr will be caught downwind.
    [(list (or 'testlist_star_expr 'testlist) elements ...)
     (ast 'nodetype "Tuple"
          'ctx (ast 'nodetype expr-ctx)
          'elts (map (lambda (e) (expr->ast e expr-ctx)) (every-other elements)))]
    
    [`(star_expr "*" ,val)
     (if (equal? expr-ctx "Store")
         (ast-of-sexp py-ragg
                      'nodetype "Starred"
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
          'args (build-formals 
                 py-ragg #f ;; straight to ast-of-sexp...
                 (match maybe-args
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
                                          [else (error-at-syntax ((get-stx) op) (format "Bad binary operator \"~a\"" op))]))))
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
                                 [else (error-at-syntax ((get-stx) (second rest)) "Unrecognized boolean operator")]))
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
                        trailers)
                       py-ragg)))
          'right (expr->ast power "Load"))]
    
    [(and (list 'power val trailers ... last-trailer)
          (not (list _ ... "**" _)))
     (wrap-with-trailer 
      last-trailer 
      expr-ctx
      (foldl 
       (lambda (trailer left-ast)
         (wrap-with-trailer trailer "Load" left-ast py-ragg))
       (expr->ast val "Load")
       trailers) 
      py-ragg)]

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
     (error-at-syntax ((get-stx) py-ragg) "Cannot mix string and bytestring literals")]

    ;; Note: True, False, None lexed as names though they're in the grammar used.
    [(list 'atom (cons type val))
     (if (equal? expr-ctx "Store")
         ;; Is this now redundant with the assignment checks?
         (error-at-syntax ((get-stx) py-ragg) "Cannot store to a literal")
         (case type
           [(integer float) (ast 'nodetype "Num" 'n val)]
           [(imaginary) (ast 'nodetype "Num" 
                             'n (ast 'nodetype "Complex" 'value val))]
           [else (error-at-syntax ((get-stx) py-ragg) "Literal value type has not been handled" #t)]))]

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
          'ctx (ast 'nodetype expr-ctx))]

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
          'ctx (ast 'nodetype expr-ctx)
          'elts (map (lambda (expr) (expr->ast expr expr-ctx)) (every-other exprs)))]

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
    
    ;; An oddity... no ctx.
    [`(atom "...")
     (ast 'nodetype "Ellipsis")]

    [_ (error-at-syntax ((get-stx) py-ragg) (format "Unhandled parse tree with LHS ~a" (first py-ragg)) #t)]))

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
(define (dotted-name->ast ragg-sexp)
  (match ragg-sexp
    [`(dotted_name (name . ,init-name) ,segments ...)
     (foldl (lambda (name-segment name-ast)
              (match name-segment
                [`(name . ,id-part) 
                 (ast 'nodetype "Attribute"
                      'attr id-part
                      'ctx (ast 'nodetype "Load")
                      'value name-ast)]
                ["." name-ast]))
            (ast-of-sexp ragg-sexp
                         'nodetype "Name"
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
         [`("<>") (error "<> operator is not supported.")] ; PEP 401. Yes, this is implemented in CPython.
         [`("is") "Is"]
         [`("is" "not") "IsNot"])))

(define (suite->ast-list suite)
  (match suite
    [(list 'suite stmt) 
     (flatten (stmt->ast-list stmt))]
    [(list 'suite "NEWLINE" "INDENT" stmts ... "DEDENT")
     (flatten (map stmt->ast-list stmts))]))

(define (wrap-with-trailer trailer expr-ctx left-ast value-sexp)
  (with-src-sexp value-sexp #:end trailer (wrap-with-trailer-concrete trailer expr-ctx left-ast)))

(define (wrap-with-trailer-concrete trailer expr-ctx left-ast)
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
              [_ (error-at-syntax ((get-stx) s) "Unhandled subscript" #t)])))
         
         (match trailer
           [`(trailer "(" ")") (build-call '() make-call-ast)]
           [`(trailer "(" (arglist ,args ...) ")") (build-call args make-call-ast)]
           [`(trailer "." (name . ,name)) (attr-ast name)]
           
           ;; Subscript lists: "Subscript" containing slice of:
           ;; 2+ subscripts, at least one slice -> "ExtSlice" w/dims as "Slice" and "Index"
           ;; 2+ subscripts, no slices -> "Index" w/value "Tuple" w/elts as values
           ;; 1 subscript, slice -> "Slice" 
           ;; 1 subscript, not slice -> "Index" w/value as value
           ;; This is probably more hairy than necessary, considering subscript->ast
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
           [_ (error-at-syntax ((get-stx) trailer) "Unrecognized call, subscript or slice" #t)])))

(define (decorator->ast decorator)
  (match decorator 
    [`(decorator "@" ,dec-name ,decorator-rest ...)
     (let ((dec-ast (dotted-name->ast dec-name)))
       (match decorator-rest
         [`("NEWLINE") dec-ast]
         [`("(" ")" "NEWLINE") (ast-of-sexp decorator
                                            'nodetype "Call"
                                            'args '()
                                            'keywords '()
                                            'kwargs #\nul
                                            'starargs #\nul
                                            'func dec-ast)]
         [`("(" (arglist ,args ...) ")" "NEWLINE")
          (build-call args (lambda (args keywords kwarg stararg)
                             (ast-of-sexp decorator
                                          'nodetype "Call"
                                          'args args
                                          'keywords keywords
                                          'kwargs kwarg
                                          'starargs stararg
                                          'func dec-ast)))]
         [_ (error-at-syntax ((get-stx) decorator) "Unrecognized decorator" #t)]))]
    [_ (error-at-syntax ((get-stx) decorator) "Unrecognized decorator" #t)]))

(define (every-other lst)
  (cond [(null? lst) '()]
        [(null? (cdr lst)) (list (car lst))]
        [else (cons (car lst) (every-other (cddr lst)))]))

;; Process arglist and call make-call-ast with args, keywords, kwarg and stararg

;; args: 'rest' of a typedargslist or varargslist sexp, or the empty list
;; make-call-ast: A function used to construct the call
;;    (make-call-ast pos-args key-args kwarg stararg),
;;   'id' AST list * 'keyword' AST list * AST * AST -> AST
(define (build-call args make-call-ast)
  (local ((define (more-args remaining-args pos-args key-args stararg kwarg)
            (match remaining-args
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
                                        'arg (ast-ref (expr->value-ast key) 'id)
                                        'value (expr->value-ast val)) key-args)
                             stararg kwarg)]
                 ;; bare generators
                 [`((argument ,expr ,comp_for))
                  ;; Must be the only argument.
                  ;; Make sure args is either (_) or (_ ",") where _ is this generator.
                  ;; This might be... a *little* bit inside out.
                  (match args
                    [(or `(,_) `(,_ ","))
                     (more-args more 
                                (cons (build-comprehension comp_for
                                                           (lambda (generators)
                                                             (ast 'nodetype "GeneratorExp"
                                                                  'elt (expr->ast expr "Load")
                                                                  'generators generators)))
                                      pos-args)
                                key-args stararg kwarg)]
                    [_ (error-at-syntax 
                        ((get-stx) (car arg-parts)) 
                        "A bare generator cannot be mixed with other arguments.")])]
                 [`((argument ,expr))
                  (more-args more (cons (expr->value-ast expr) pos-args) key-args stararg kwarg)]
                 [_ (error-at-syntax ((get-stx) (car remaining-args)) "Error parsing arguments." #t)])])))
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


;; Build the 'arguments' AST for formal arguments of functiondefs and lambdas
;; Currently covers both typedargslist and varargslist
;; start-sexp, end-sexp: ragg sexps for position in general error reporting
;; args: list of ragg sexp (NOT a ragg sexp itself!)
(define (build-formals start-sexp end-sexp arg-list) 
  (local (;; This needs either a macro or better taste.
          ;; A bunch of accumulators masquerading as a data structure
          ;; These are all final AST values - ASTs, AST lists (reversed), strings
          (struct Formals (args defaults vararg-name kwarg-name kwargs kw-defaults vararg-annotation kwarg-annotation))
          ;; Take an fpdef and return the name string and annotation AST (can be #\nul)
          (define (fpdef-values d)
            (match d
              [`(,_ (name . ,name)) (values name #\nul)]
              [`(,_ (name . ,name) ":" ,annotation) (values name (expr->value-ast annotation))]))
          (define (*fpdef->ast n)
            (let-values (((name anno) (fpdef-values n)))
              (ast-of-sexp n
                           'nodetype "arg"
                           'annotation anno
                           'arg name)))
          ;; formals here is Formals, not a ragg sexp.
          (define (formals->ast formals)
            (match-let (((struct Formals (args defaults vararg-name kwarg-name
                                               kwargs kw-defaults vararg-annotation kwarg-annotation)) 
                         formals))
                       (check-names (append* (list kwarg-name vararg-name) (map arg-name kwargs) (map arg-name args) '()))
                       (ast-of-sexp start-sexp #:end end-sexp
                                    'nodetype "arguments"
                                    'args (reverse args)
                                    'defaults (reverse defaults) ;; Defaults for only optional args
                                    'vararg vararg-name
                                    'kwargannotation kwarg-annotation
                                    'kwarg kwarg-name
                                    'varargannotation vararg-annotation
                                    'kw_defaults (reverse kw-defaults) ;; A default for each kwarg (#\nul if none)
                                    'kwonlyargs (reverse kwargs))))

          (define (arg-name arg)
            (match arg
              [(hash-table ('nodetype "SrcLoc") ('ast inner-ast) (_ _) ...) (arg-name inner-ast)]
              [(hash-table ('nodetype "arg") ('arg name) (_ _) ...) name]))

          ;; Take a list of names that can be a string or #\nul. Error if there are duplicates other than #\nul.
          (define (check-names lst)
            (let ((actual-names (remove* '(#\nul) lst)))
              (unless (equal? actual-names (remove-duplicates actual-names))
                (error-at-syntax ((get-stx) start-sexp) "Argument names cannot be duplicated."))))
          
          (define (more-args args kw? formals)
            (match args
              [`() 
               (if (and kw? 
                        (null? (Formals-kwargs formals)) 
                        (equal? #\nul (Formals-vararg-name formals)))
                   ;; A somewhat late and dirty way to catch this
                   ;; Potentially confusing source position
                   (error-at-syntax ((get-stx) (last arg-list)) "'*' must be followed by at least one named argument")
                   (formals->ast formals))]
              [`("," ,rest ...) (more-args rest kw? formals)]
              [(list (and arg-parts (not ",")) ... rest ...)
               (match arg-parts
                 [`("*" ,fpdef)
                  (let-values (((name anno) (fpdef-values fpdef)))
                    (more-args rest #t (struct-copy Formals formals [vararg-name name] [vararg-annotation anno])))]
                 [`("*")
                  (more-args rest #t formals)]
                 [`("**" ,fpdef)
                  (let-values (((name anno) (fpdef-values fpdef)))
                    (more-args rest kw? (struct-copy Formals formals [kwarg-name name] [kwarg-annotation anno])))]
                 [`(,fpdef "=" ,default-expr)
                  (more-args rest kw?
                             (if kw?
                                 (struct-copy Formals formals 
                                              [kwargs (cons (*fpdef->ast fpdef) (Formals-kwargs formals))]
                                              [kw-defaults
                                               (cons (expr->ast default-expr "Load") 
                                                     (Formals-kw-defaults formals))])
                                 (struct-copy Formals formals 
                                              [args (cons (*fpdef->ast fpdef) (Formals-args formals))]
                                              [defaults
                                                (cons (expr->ast default-expr "Load") 
                                                      (Formals-defaults formals))])))]
                 [`(,fpdef)
                  (more-args rest kw?
                             (if kw?
                                 (struct-copy Formals formals 
                                              [kwargs (cons (*fpdef->ast fpdef) (Formals-kwargs formals))]
                                              [kw-defaults 
                                               (cons #\nul (Formals-kw-defaults formals))])
                                 (struct-copy Formals formals 
                                              [args (cons (*fpdef->ast fpdef) (Formals-args formals))])))]
                 [_ (error-at-syntax ((get-stx) (first args)) "Unhandled formal argument" #t)])])))
         (more-args arg-list #f (Formals '() '() #\nul #\nul '() '() #\nul #\nul))))

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
              [_ (error-at-syntax ((get-stx) (first comp)) "Unhandled comprehension clause" #t)])))
         (match comp 
           [`(comp_for "for" ,target-expr "in" ,iter-expr ,more ...)
            (more-clauses more '() target-expr iter-expr '())]
           [_ (error-at-syntax ((get-stx) comp) "Unhandled comprehension clause" #t)])))

#|
There are a few cases handled in this file by messing with ASTs instead of just producing them, since ASTs are simpler to recurse on.
a) Validating assign/delete/generator targets
b) ast-ref, for Getting a name from the odd case in the grammar where a test is expected but a name is needed.
|#

;; AST should be a SrcLoc AST. Show file position, then msg.
(define (error-at-ast ast msg)
  (match ast
    [(hash-table ('nodetype "SrcLoc")
                 ('line line)
                 ('column column)
                 ('position position)
                 ('span span)
                 ('ast _))
     (error (format "Syntax error, line ~a, column ~a:~n~a" line column msg))]
    [_ (error (format "Syntax error, unknown position:~n~a" msg))]))

;; If the AST is not a valid target for an assignment, error at that AST
(define (validate-assign-target ast in-list? at-ast)
  (match ast
    [(hash-table ('nodetype "SrcLoc") ('ast inner-ast) (_ _) ...) 
     (validate-assign-target inner-ast in-list? ast)]
    [(hash-table ('nodetype "Name") (_ _) ...) #t]
    [(hash-table ('nodetype "Attribute") (_ _) ...) #t]
    [(hash-table ('nodetype "Subscript") (_ _) ...) #t]
    [(hash-table ('nodetype "Slice") (_ _) ...) #t]
    [(hash-table ('nodetype "ExtSlice") (_ _) ...) #t]
    [(hash-table ('nodetype "Starred") (_ _) ...) 
     (if in-list? #t (error-at-ast at-ast "Starred assignment target must be in list or tuple"))]
    [(or (hash-table ('nodetype "Tuple") ('elts elts) (_ _) ...)
         (hash-table ('nodetype "List") ('elts elts) (_ _) ...))
     (andmap (lambda (elt) (validate-assign-target elt #t at-ast)) elts)]
    [_ (error-at-ast at-ast (format "Invalid assignment target, type ~a" (hash-ref ast 'nodetype)))]))

;; This is a bit muddled. It's only for "Del", but calls into validate-*assign*-target...
(define (validate-target-list ast-list)
  (map (lambda (a) (validate-assign-target a #f a)) ast-list))

;; Duplicated in get-structured-python  
;; Get concrete (non-srcloc) property of AST ast identified by key (symbol) by skipping SrcLoc nodes.
(define (ast-ref ast key)
  (cond
   [(equal? "SrcLoc" (hash-ref ast 'nodetype))
    (ast-ref (hash-ref ast 'ast) key)]
   [else (hash-ref ast key)]))
