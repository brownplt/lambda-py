#lang plai-typed/untyped
(require "python-syntax.rkt"
         "python-core-syntax.rkt"
         "python-lib-bindings.rkt"
         "python-lexical-syntax.rkt"
         "python-syntax-operations.rkt")
(require "util.rkt")
(require [typed-in racket (format : (string 'a -> string))])
(require [typed-in racket (member : ('a (listof 'a) -> boolean))])
(require [typed-in racket (flatten : ((listof (listof 'a) ) -> (listof 'b)))])
(require [typed-in racket (remove-duplicates : ((listof 'a) -> (listof 'a)))])
(require [typed-in racket (gensym : (symbol -> symbol))])
(require [typed-in racket (substring : (string number number -> string))])
(require [typed-in racket (string-length : (string -> number))])


;phase2 - desugaring to get rid of instance variables, decorators, and defaults.
(define (phase2 expr)
  (LexModule
   (list 
    (optimization-pass
     (let-phase
       (post-desugar
        (remove-blocks
         expr)))))))

(define (post-desugar [expr : LexExpr]) : LexExpr
    (local
     [
      (define (finish-hoist-functions expr)
        (lexexpr-modify-tree
         expr
         (lambda (y)
           (type-case LexExpr y
             [LexFunc (name args defaults body decorators class)
                      (if
                       (and
                        (> (string-length (symbol->string name )) (string-length "class-replacement"))
                        (equal? "class-replacement" (substring (symbol->string name) 0 (string-length "class-replacement"))))
                       (LexFunc name args defaults body decorators (none))
                       y)]
             [else (default-recur)]))))
      (define hoist-functions (local
       [
      ;takes a body, makes two scopes out of it -
      ;one for the functions, one for everything else.
      ;alejandro's locals-in-class-definition solution.
      ;this is currently _very_ slow.
        (define (create-bindings-helper functions ids) : (listof LexExpr)
          (cond
           [(empty? functions) empty]
           [(empty? ids) empty]
           [else (cons
                  (LexAssign (list (LexLocalId (first ids) 'Store)) (first functions))
                  (create-bindings-helper (rest functions) (rest ids)))]))
        (define (create-bindings functions ids)
          (LexSeq (create-bindings-helper functions ids))
          )
        (define list-of-functions empty)
        (define list-of-identifiers empty)
        (define (hoist-functions [expr : LexExpr])
          (begin
            ;(display "entering hoist-functions")
          (let ((result (let ((replaced-body (replace-functions expr)))
            (introduce-locals
             list-of-identifiers
             (LexSeq
              (list
               ;this is the random-identifiers-assigned-to-lambdas part
               (create-bindings list-of-functions list-of-identifiers)
               ;this is the new body.
               replaced-body
               ))
              ))))
            (begin
              ;(display "exiting hoist-functions")
              result)
            )
            ))

        (define (make-local-ids args)
          (map (lambda (y) (LexLocalId y 'Load)) args))
                                        ;functions are lam, func, funcvararg.
      (define (replace-functions [body2 : LexExpr])
        (lexexpr-modify-tree
         body2
         (lambda
             ([e : LexExpr])
           (type-case LexExpr e
             [LexFunc (name args defaults body decorators class)
                      (LexFunc name args
                               (map replace-functions defaults)
                               (begin
                                 (set! list-of-identifiers (cons (generate-identifier) list-of-identifiers))
                                 (set! list-of-functions (cons
                                                          
                                                          (LexFunc (first list-of-identifiers)
                                                                   args
                                                                   empty
                                                                   body
                                                                   empty
                                                                   (none)) list-of-functions))
                                 (LexReturn (LexApp (LexLocalId (first list-of-identifiers) 'Load) (make-local-ids args)))
                                 )
                               (map replace-functions decorators) class)
                      ]
             [LexFuncVarArg (name args sarg body decorators class)
                            (LexFuncVarArg name args sarg
                               (begin
                                 (set! list-of-identifiers (cons (generate-identifier) list-of-identifiers))
                                 (set! list-of-functions (cons
                                                          (LexFunc (first list-of-identifiers)
                                                                   (cons sarg args)
                                                                   empty
                                                                   body
                                                                   empty
                                                                   (none))
                                                          list-of-functions))
                                 (LexReturn
                                  (LexApp (LexLocalId (first list-of-identifiers) 'Load) (make-local-ids (cons sarg args))))
                                 )
                               (map replace-functions decorators) class)]
             [LexLam (args body) (begin
                                   (set! list-of-identifiers (cons (generate-identifier) list-of-identifiers))
                                   (set! list-of-functions (cons e list-of-functions))
                                   (LexLocalId (first list-of-identifiers) 'Load))]
             ;[LexBlock (nls es) e]
             [LexClass (scope name bases body) e]
             [else (default-recur)]))))
      (define (generate-identifier )
        (gensym 'class-replacement))
      ]
       hoist-functions))
      ;end hoist functions.
      
      (define (replace-instance-and-annotate-methods-with-class expr class-expr)
        (let ((recur (lambda (x) (replace-instance-and-annotate-methods-with-class x class-expr))))
          (lexexpr-modify-tree
           expr
           (lambda (y)
             (type-case LexExpr y
               [LexInstanceId (x ctx) (LexDotField class-expr x)]
               [LexFunc (name args defaults body decorators class)
                        (LexFunc name args (map recur defaults) body decorators
                                 (some class-expr))]
               [LexFuncVarArg (name args sarg body decorators class)
                              (LexFuncVarArg name args sarg body (map recur decorators) (some class-expr)) ]
               [LexClass (scope name bases body) (LexClass scope name bases body)]
               [else (default-recur)])))))
      #;(define (annotate-methods-with-class expr [classname : LexExpr])
        (lexexpr-modify-tree
         expr
         (lambda (y)
           (type-case LexExpr y
             [LexFunc (name args defaults body decorators class)
                      (LexFunc name args defaults body decorators (some classname))]
             [LexFuncVarArg (name args sarg body decorators class)
                            (LexFuncVarArg name args sarg body decorators (some classname)) ]
             [LexClass (scope name bases body)
                       (LexClass scope name bases (annotate-methods-with-class body name))]
             [else (default-recur)]))))
      (define (split-instance-into-instance-locals expr)
        (introduce-locals
         (lexexpr-fold-tree expr (lambda (y)
                                   (type-case LexExpr y
                                     [LexInstanceId (x ctx) (list x)]
                                     [LexClass (scope name bases body) empty]
                                     [LexBlock (nls es) empty]
                                     [else (default-recur)])))
         (split-instance-into-instance-locals-helper expr)))
      (define (split-instance-into-instance-locals-helper expr)
        (let ((find-var (lambda (expr) : symbol
                          (let ((var (lexexpr-fold-tree
                           expr
                           (lambda (e)
                             (type-case LexExpr e
                               [LexInstanceId (x ctx) (list x)]
                               [LexBlock (nls e) empty]
                               [LexClass (scope name bases body) empty]
                               [else (default-recur)])))))
                            (cond
                             [(empty? var) (error 'split-instance-lambda "we didn't find an instance variable")]
                             [(empty? (rest var)) (first var)]
                             [else (error 'split-instance-lambda "can't handle multiple instance variables!")])))))
          (lexexpr-modify-tree
           expr
           (lambda (e)
             (type-case LexExpr e
               [LexAssign (targets value)
                          (let ((affected-var (map find-var targets)))
                            (cond
                             [(empty? affected-var) (error 'e "wait we should have caught that just above")]
                             [(empty? (rest affected-var))
                              (LexSeq
                               (list
                                (LexAssign (list (LexLocalId (first affected-var) 'Store)) value)
                                (LexAssign targets (LexLocalId (first affected-var) 'Load))))
                              ]
                             [else (error 'e "can't handle multiple assignment yet.")]))]
                                        ;[LexAugAssign (op target value )]
               [LexBlock (nls es) e]
               [LexClass (scope name bases body) e]
               [else (default-recur)])))))
      (define (deal-with-class expr class-expr)
        (begin 
        ;(display (string-append "deal-with-class on " (to-string expr)))
        (lexexpr-modify-tree
         expr
         (lambda ([y : LexExpr])
           (type-case LexExpr y
             [LexClass (scope name bases body)
                       (let ((body (hoist-functions (split-instance-into-instance-locals body))))
                       ;(let ((body (identity body)))
                           (let ((class-expr (if (Instance-scoped? scope)
                                                 (LexDotField class-expr name)
                                                     class-expr)))
                             (let ((new-body (replace-instance-and-annotate-methods-with-class
                                              body
                                              class-expr
                                              )))
                               (LexSeq (list (LexAssign
                                              (list class-expr)
                                              (LexClass scope name bases (LexPass)))
                                             (deal-with-class
                                              new-body
                                              class-expr))))))]
             [else (default-recur)])))))
          (define (top-level-deal-with-class expr)
            (begin
              ;(display (string-append "top-level-deal-with-class on " (string-append (to-string expr) "\n") ))
            (lexexpr-modify-tree
             expr
             (lambda ([y : LexExpr])
               (type-case LexExpr y
                 [LexClass (scope name bases body)
                           (finish-hoist-functions (if (Instance-scoped? scope)
                                 (error 'lexical "instance is not inside class")
                                 (deal-with-class
                                  y
                                  (if (Globally-scoped? scope)
                                      (LexGlobalId name 'Load)
                                      (LexLocalId name 'Load)))))]
                 [else (default-recur)])))))]
        (top-level-deal-with-class expr)))

(define (let-phase [expr : LexExpr] ) : LexExpr
(collapse-pyseq (cascade-undefined-globals (list-subtract
                                            (begin
                                              ;(display (extract-post-transform-globals expr))
                                              (extract-post-transform-globals expr))
                                            library-names) expr))) ;all globals, not just the current scope

(define library-names (map (lambda (b) (bind-left b)) lib-function-dummies)) 

(define (cascade-undefined-globals [globals : (listof symbol) ] [body : LexExpr] ) : LexExpr
  (LexGlobals globals body))

(define (extract-post-transform-globals expr) : (listof symbol)
  (remove-duplicates
   (lexexpr-fold-tree
    expr
    (lambda (exp)
      (type-case LexExpr exp
        [LexGlobalId (x ctx) (list x)]
        ;[LexAssign (lhs rhs) (map (lambda (y) (LexGlobalId-x y))
        ;                            (filter (lambda (y) (LexGlobalId? y)) lhs))]
        [else (default-recur)])))))

(define (introduce-locals listof-locs expr)
        (cond
         [(empty? listof-locs) expr]
         [else (LexLocalLet (first listof-locs) (LexUndefined)
                            (introduce-locals (rest listof-locs) expr))]))

;stub
(define (optimization-pass expr)
  expr)



(define (collapse-pyseq expr ) 
  (lexexpr-modify-tree
   expr 
   (lambda (x) (type-case LexExpr x
                 [LexSeq(lis)
                        (LexSeq (flatten (map (lambda (y)
                                                (let ((e (collapse-pyseq y)))
                                                  (if (LexSeq? e) (LexSeq-es e) (list e))))
                                              lis)))]
                 [else (default-recur)]))))

(define (remove-blocks expr)
  (lexexpr-modify-tree
   expr
   (lambda (x)
     (type-case LexExpr x
       [LexBlock (nls e) (remove-blocks e)]
       [else (default-recur)]))))

(define (collect-locals-in-scope expr starting-locals) : (listof symbol)
  (flatten
   (list
    (lexexpr-fold-tree
     expr
     (lambda (y)
       (type-case LexExpr y
         [LexBlock (_ __) empty]
         [LexLocalId (x ctx) (list x)]
         [else (default-recur)])))
    starting-locals)))

(define (make-local-list starting-locals expr)
  (lexexpr-modify-tree
   expr
   (lambda (y)
     (type-case LexExpr y
       [LexBlock (nls body)
                 (let ((locals (collect-locals-in-scope body starting-locals)))
                   (LexBlock nls (LexSeq (list (LexInScopeLocals locals)
                                               (make-local-list locals body)))))]
       [else (default-recur)]))))

