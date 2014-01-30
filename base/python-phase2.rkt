#lang plai-typed/untyped
(require "python-syntax.rkt"
         "python-core-syntax.rkt"
         "python-lib-bindings.rkt"
         "python-lexical-printer.rkt"
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
    (LexPass)
	;(LexInScopeLocals empty)
                                        ;(phase2-locals empty)
    (remove-blocks
     (replace-lexinscopelocals
      (optimization-pass
       (let-phase
         (remove-nonlocal
          (collapse-pyseq

            (post-desugar
             (make-local-list
              empty
              expr)))))))))))



;;wholly and utterly for debugging.
(define (phase2-without-locals expr)
  (begin
    (LexModule
   (list
    (LexPass)
	;(LexInScopeLocals empty)
                                        ;(phase2-locals empty)
     (optimization-pass
      (let-phase
          (collapse-pyseq
           (post-desugar
            (make-local-list
             empty
             expr))))))))
  )

(define (globals-fun expr)
  (let ((globals (lexexpr-fold-tree
                  expr
                  (lambda (e) (if (LexGlobalId? e)
                                  (list (LexGlobalId-x e))
                                  (default-recur))))))
    (LexSeq (list
             (LexAssign (list (LexGlobalId '%globals 'Store))
                        (LexFunc '%globals empty (none) empty (none) empty empty
                                 (collecting-ids-body globals
                                                      (lambda (x y) (LexGlobalId x y)))
                                 empty (none)))
             (LexAssign (list (LexGlobalId '%locals 'Store))
                        (LexGlobalId '%globals 'Load))
                        
             expr))))

(define (post-desugar [expr : LexExpr]) : LexExpr
    (local
     [
      (define (finish-hoist-functions expr)
        (lexexpr-modify-tree
         expr
         (lambda (y)
           (type-case LexExpr y
             [LexFunc (name args vararg kwonlyargs kwarg defaults kw_defaults body decorators class)
                      (if
                       (and
                        (> (string-length (symbol->string name )) (string-length "class-replacement"))
                        (equal? "class-replacement" (substring (symbol->string name) 0 (string-length "class-replacement"))))
                       (LexFunc name args vararg kwonlyargs kwarg defaults kw_defaults body decorators (none))
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
            (set! list-of-functions empty)
            (set! list-of-identifiers empty)
          (let ((result (let ((replaced-body (replace-functions expr)))
            (introduce-locals
             list-of-identifiers
             (LexSeq
              (list
               ;this is the random-identifiers-assigned-to-lambdas part
               (create-bindings list-of-functions list-of-identifiers)
               ;this is the new body.
               (split-instance-into-instance-locals (LexBlock empty replaced-body))
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
             [LexFunc (name args vararg kwonlyargs kwarg defaults kw_defaults body decorators class)
                      (let ([all-args (flatten (list args (option->list vararg) kwonlyargs (option->list kwarg)))])
                        (LexFunc name args vararg kwonlyargs kwarg
                                 (map replace-functions defaults)
                                 (map replace-functions kw_defaults)
                                 (begin
                                   (set! list-of-identifiers (cons (generate-identifier) list-of-identifiers))
                                   (set! list-of-functions (cons
                                                          
                                                            (LexFunc (first list-of-identifiers)
                                                                     all-args
                                                                     (none)
                                                                     empty
                                                                     (none)
                                                                     empty
                                                                     empty
                                                                     body
                                                                     empty
                                                                     (none)) list-of-functions))
                                   (LexBlock empty (LexReturn (some (LexApp (LexLocalId (first list-of-identifiers) 'Load)
                                                                            (make-local-ids all-args)
                                                                            (list) (none) (none)))))
                                   )
                                 (map replace-functions decorators) class))
                      ]
             [LexLam (args vararg kwonlyargs kwarg defaults kw_defaults body) (begin
                                   (set! list-of-identifiers (cons (generate-identifier) list-of-identifiers))
                                   (set! list-of-functions (cons e list-of-functions))
                                   (LexLocalId (first list-of-identifiers) 'Load))]
             ;[LexBlock (nls es) e]
             [LexClass (scope name bases body keywords stararg kwarg decorators) e]
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
               [LexFunc (name args vararg kwonlyargs kwarg defaults kw_defaults body decorators class)
                        (LexFunc name args vararg kwonlyargs kwarg
                                 (map recur defaults) (map recur kw_defaults)
                                 body decorators
                                 (some class-expr))]
               [LexClass (scope name bases body keywords stararg kwarg decorators)
                         (LexClass scope name bases body keywords stararg kwarg decorators)]
               [else (default-recur)])))))
      (define (split-instance-into-instance-locals expr)
        (run-after-blocks
         expr
         (lambda (expr)
           (begin
             ;(display "entering split-instance-into-instance-locals\n")
             ;(lexexpr-print expr)
             
             (introduce-locals
              (lexexpr-fold-tree expr (lambda (y)
                                        (type-case LexExpr y
                                          [LexInstanceId (x ctx) (list x)]
                                          [LexClass (scope name bases body keywords stararg kwarg decorators)
                                                    empty]
                                          [LexBlock (nls es) empty]
                                          [else (default-recur)])))
              (split-instance-into-instance-locals-helper expr))))))
      (define (split-instance-into-instance-locals-helper expr)
        (let ((find-var (lambda ([expr : (listof LexExpr)]) : (listof symbol)
                          (let ((var
                             (flatten (map (lambda (expr) : (listof symbol)
                         (lexexpr-fold-tree
                           expr
                           (lambda (e)
                             (type-case LexExpr e
                               [LexInstanceId (x ctx) (list x)]
                               [LexBlock (nls e) empty]
                               [LexClass (scope name bases body keywords stararg kwarg decorators)
                                         empty]
                               [else (default-recur)])))) expr))))
                            var))))
          (lexexpr-modify-tree
           expr
           (lambda (e)
             (type-case LexExpr e
               [LexAssign (targets value)
                          (let ((affected-var (find-var targets)))
                            (cond
                             [(empty? affected-var) (default-recur)]
                             [(empty? (rest affected-var))
                              (LexSeq
                               (list
                                (LexAssign (list (LexLocalId (first affected-var) 'Store)) value)
                                (LexAssign targets (LexLocalId (first affected-var) 'Load))))
                              ]
                             [else
                              (type-case LexExpr (first targets)
                                [LexTuple (targs)
                                          (LexSeq
                                           (flatten
                                            (list
                                             (list
                                              (LexAssign
                                               (list
                                                (LexTuple (map
                                                           (lambda (y) (LexLocalId y 'Store))
                                                           affected-var))) value))
                                             (2-map (lambda ([a : LexExpr] [b : symbol])
                                                      (LexAssign (list a) (LexLocalId b 'Load))
                                                      )
                                                    targs affected-var)))) ]
                                [else (error 'split-instance "we don't handle multiple targets.")]
                             #|[else (error 'e (string-append
                                              "can't handle multiple assignment yet."
                                              (to-string affected-var)))];|#
                             )]))]
                                        ;[LexAugAssign (op target value )]
               [LexFor (target iter body orelse) (type-case LexExpr target
                                                   [LexInstanceId
                                                    (x ctx)
                                                    (LexSeq
                                                     (list
                                                      (LexFor
                                                       (LexLocalId x ctx)
                                                       (split-instance-into-instance-locals-helper iter)
                                                       (split-instance-into-instance-locals-helper body)
                                                       (split-instance-into-instance-locals-helper orelse))
                                                      (LexAssign (list target) (LexLocalId x 'Load))))
                                                    ]
                                                   [else (default-recur)])]
               [LexBlock (nls es) e]
               [LexClass (scope name bases body keywords stararg kwarg decorators) e]
               [else (default-recur)])))))

      (define (2-map fun arg1 arg2)
        (cond
         [(and (empty? arg1) (empty? arg2)) empty]
         [(or (and (empty? arg1) (not (empty? arg2)))
              (and (not (empty? arg1)) (empty? arg2)))
          (error '2-map "argument lists must be of same length")]
         [else (cons (fun (first arg1) (first arg2)) (2-map fun (rest arg1) (rest arg2)))]))
           
      
      (define (deal-with-class expr class-expr)
        (begin 
        ;(display (string-append "deal-with-class on " (to-string expr)))
        (lexexpr-modify-tree
         expr
         (lambda ([y : LexExpr])
           (type-case LexExpr y
             [LexClass (scope name bases body keywords stararg kwarg decorators)
                       (let ((body
                              (type-case LexExpr body
                                [LexBlock (nls es) (LexBlock nls (hoist-functions es))]
                                [else (error 'deal-with-class "Thing inside LexClass not a LexBlock!")])
                              ))
                           (let ((class-expr (if (Instance-scoped? scope)
                                                 (LexDotField class-expr name)
                                                     class-expr)))
                             (let ((new-body (replace-instance-and-annotate-methods-with-class
                                              body
                                              class-expr
                                              )))
                               (LexAssign
                                (list (type-case LocalOrGlobal scope
                                        [Locally-scoped [] (LexLocalId name 'Store)]
                                        [Globally-scoped [] (LexGlobalId name 'Store)]
                                        [else (error 'e "should be no more instance scope!")]))
                                (LexClass scope name bases
                                          (deal-with-class new-body class-expr)
                                          keywords stararg kwarg decorators)))))]
             [else (default-recur)])))))
          (define (top-level-deal-with-class expr)
            (begin
              ;(display (string-append "top-level-deal-with-class on " (string-append (to-string expr) "\n") ))
            (lexexpr-modify-tree
             expr
             (lambda ([y : LexExpr])
               (type-case LexExpr y
                 [LexClass (scope name bases body keywords stararg kwarg decorators)
                           (finish-hoist-functions (if (Instance-scoped? scope)
                                 (error 'lexical "instance is not inside class")
                                 (deal-with-class
                                  y
                                  (if (Globally-scoped? scope)
                                      (LexGlobalId name 'Load)
                                      (LexLocalId name 'Load)))))]
                 [else (default-recur)])))))
          (define (replace-classes [expr : LexExpr])
          (lexexpr-modify-tree expr (lambda (e)
          (type-case LexExpr e
            [LexClass (scope name bases body keywords stararg kwarg decorators)
                      (type-case LocalOrGlobal scope
                        [Instance-scoped
                         []
                         (LexAssign
                          (list
                           (LexInstanceId name 'Store))
                       (LexApp (LexLam empty (none) empty (none) empty empty
                                       (LexLocalLet
                                        name (LexUndefined)
                                        (LexSeq
                                         (list
                                          (LexClass (Locally-scoped) name
                                                    (map replace-classes bases)
                                                    (replace-classes body)
                                                    (map replace-classes keywords)
                                                    (option-map replace-classes stararg)
                                                    (option-map replace-classes kwarg)
                                                    (map replace-classes decorators))
                                          (LexLocalId name 'Load)))))
                               (list) (list) (none) (none)))]
                        [else (default-recur)])]
                      
            [else (default-recur)]))))]
        (top-level-deal-with-class (replace-classes  expr))))

(define (let-phase [expr : LexExpr] ) : LexExpr
(collapse-pyseq
 (cascade-undefined-globals (list-subtract
                             (begin
                                        ;(display (extract-post-transform-globals expr))
                               (extract-post-transform-globals expr))
                             library-names) expr)
 )
) ;all globals, not just the current scope

(define library-names (map (lambda (b) (bind-left b)) lib-function-dummies)) 

(define (run-after-blocks expr fun)
  (type-case LexExpr (collapse-pyseq expr)
    [LexBlock (nls e) (LexBlock nls (run-after-blocks e fun))]
    [else (fun expr)]))

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


(define (non-lexpass exprs )
  (filter (lambda (y) (not (LexPass? y))) exprs))

(define (collapse-pyseq expr )
  (lexexpr-modify-tree
   expr 
   (lambda (x) (type-case LexExpr x
                 [LexSeq(lis)
                        (let ((new-es (non-lexpass (flatten (map (lambda (y)
                                                (let ((e (collapse-pyseq y)))
                                                  (if (LexSeq? e) (LexSeq-es e) (list e))))
                                              lis)))))
                          (cond
                           [(empty? new-es) (LexPass)]
                           [(empty? (rest new-es)) (first new-es)]
                           [else (LexSeq new-es)]))]
                 [else (default-recur)]))))

(define (remove-nonlocal expr)
  (lexexpr-modify-tree
   expr
   (lambda (x)
     (type-case LexExpr x
       [PyLexNonLocal(y) (LexPass)]
       [else (default-recur)]))))


(define (remove-blocks expr)
  (lexexpr-modify-tree
   expr
   (lambda (x)
     (type-case LexExpr x
       [LexBlock (nls e) (remove-blocks e)]
       [else (default-recur)]))))


;BEGIN LOCALS CODE


(define (move-past-local-lets-helper [new-expr : LexExpr]
                                     [listof-exprs : (listof LexExpr)]) :
                                     (listof LexExpr)
  (cond
   [(empty? listof-exprs) (list new-expr)]
   [(LexLocalLet? (first listof-exprs)) (cons (first listof-exprs)
               (move-past-local-lets-helper new-expr (rest listof-exprs)))]
   ;[(LexSeq? (first listof-exprs)) (cons
   ;                                 (LexSeq (move-past-local-lets-helper
   ;                                          new-expr
   ;                                          (LexSeq-es (first listof-exprs))))
   ;                                 (rest listof-exprs))]
   [else (cons new-expr listof-exprs)]))

(define (move-past-local-lets [new-expr : LexExpr]
                              [potential-local-lets : LexExpr] ) : LexExpr
  (type-case LexExpr potential-local-lets
    [LexLocalLet (id bind body)
                 (LexLocalLet id bind (move-past-local-lets new-expr body)) ]
    [LexSeq (es) (LexSeq (move-past-local-lets-helper new-expr es))]
    [else (LexSeq (list new-expr potential-local-lets))]))

(define (move-past-LexExcept new-expr [potential-excepts : LexExpr])
  (type-case LexExpr potential-excepts
    [LexExcept (types body)
               (LexExcept types (move-past-LexExcept new-expr body))]
    [LexExceptAs (types name body)
                 (LexExceptAs types name (move-past-LexExcept new-expr body))]
    [LexTryFinally (try finally) (LexTryFinally (move-past-LexExcept new-expr try)
                                                (move-past-LexExcept new-expr finally))]
    [else (cond
           [(LexPass? potential-excepts) new-expr]
           [else (LexSeq (list new-expr (begin
                                   ;(display (string-append (to-string potential-excepts) "\n"))
                                   potential-excepts)))])]))


#;fg
(define (pairs->tupleargs [keys : (listof CExpr)] [values : (listof CExpr)] )  : (listof CExpr)
  (cond
   [(empty? keys) empty]
   [(cons? keys)
    (cons (CTuple (CId '%tuple (GlobalId))
                  (list (first keys)
                        (first values)))
          (pairs->tupleargs (rest keys) (rest values)))]
   [else (error 'pairs->tupleargs "shouldn't get here")]))


(define (replace-lexinscopelocals expr)
  (lexexpr-modify-tree
   expr
   (lambda (y)
     (type-case LexExpr y
       [LexInScopeLocals (ids) (phase2-locals ids)]
       [LexClass (scope name bases body keywords stararg kwarg decorators)
                 (LexClass scope name (map replace-lexinscopelocals bases)
                           (replace-lexinscopelocals (store-locals body))
                           (map replace-lexinscopelocals keywords)
                           (option-map replace-lexinscopelocals stararg)
                           (option-map replace-lexinscopelocals kwarg)
                           (map replace-lexinscopelocals decorators))]
       [LexReturn (v?) (type-case (optionof LexExpr) v?
                        [some (v) (LexLocalLet 'return-cleanup (replace-lexinscopelocals v)
                                               (LexSeq
                                                (list
                                                 restore-locals
                                                 (LexReturn (some (LexLocalId 'return-cleanup 'Load))))))]
                        [none () (LexSeq (list restore-locals (LexReturn (none))))])]
       [LexLam (args vararg kwonlyargs kwarg defaults kw_defaults body)
               (LexLam args vararg kwonlyargs kwarg
                       (map replace-lexinscopelocals defaults)
                       (map replace-lexinscopelocals kw_defaults)
                       (replace-lexinscopelocals (store-locals-careful body)))]
       ;I'm really not sure what's going on here....
       [LexFunc (name args vararg kwonlyargs kwarg defaults kw_defaults body decoratos class)
                (LexFunc
                 name
                 args
                 vararg
                 kwonlyargs
                 kwarg
                 (map replace-lexinscopelocals defaults)
                 (map replace-lexinscopelocals kw_defaults)
                 (replace-lexinscopelocals (store-locals body))
                 (map replace-lexinscopelocals decoratos)
                 (option-map replace-lexinscopelocals class))]
       [else (default-recur)]))))



;this is the same a desugar-locals.  I'm moving things directly into this file.
;largely for ease of testing (I can read this code; desugared code not so much)
(define (phase2-locals [ids : (listof symbol)]) : LexExpr
  (LexAssign (list (LexGlobalId '%locals 'Store)) 
			 (LexFunc '%locals empty (none) empty (none) empty empty
                      (collecting-ids-body ids (lambda (x y) (LexLocalId x y)))
                      empty (none))))

(define (collecting-ids-body ids thisid)
  (LexLocalLet
   'collecting-locals (LexDict empty empty)
   (LexSeq
    (flatten
     (list
      (map
       (lambda (id)
         (LexTryExceptElse
          (thisid id 'Load)
          (list
           (LexExcept
            empty
            (LexPass)
            ))
          (LexAssign
           (list
            (LexSubscript (LexLocalId 'collecting-locals 'Load) 'Store (LexStr (symbol->string id) )))
           (thisid id 'Load))))
       ids)
      (list 
       (LexReturn (some (LexLocalId 'collecting-locals 'Load)))))))))
  
;for lambda
(define (store-locals-careful expr)
  (LexLocalLet '%locals-save (LexGlobalId '%locals 'Load)
                                        ;only for things without the word "return" in them.  -_-.
               (LexLocalLet '%return-tmp expr
                            (LexSeq (list
                                     restore-locals
                                     (LexLocalId '%return-tmp 'Load)
                                     )))))

(define (store-locals expr)
  (LexLocalLet '%locals-save (LexGlobalId '%locals 'Load)
               (LexSeq (list expr restore-locals))))


(define restore-locals
  (LexAssign (list (LexGlobalId '%locals 'Store))
             (LexLocalId '%locals-save 'Load)))


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

(define (collect-instance-in-scope expr starting-locals) : (listof symbol)
  (flatten
   (list
    (lexexpr-fold-tree
     expr
     (lambda (y)
       (type-case LexExpr y
         [LexBlock (_ __) empty]
         [LexClass (scope name bases bodyx keywords stararg kwarg decorators) empty]
         [LexInstanceId (x ctx) (list x)]
         [else (default-recur)])))
    starting-locals)))

(define (filter-locals [ids : (listof symbol)])
  (remove-duplicates (filter
   (lambda (y)
     (let ((str (symbol->string y)))
       (not
        (or (contains-char? str (chr "-"))
            (contains-str? str "__")
            (contains-char? str (chr "%"))
            (contains-char? str (chr "$"))
            )
        )
       )) ids)))

(define (make-local-list [starting-locals : (listof symbol)] [expr : LexExpr] ) : LexExpr
                         
    (lexexpr-modify-tree
     expr
     (lambda (y)
       (let ((recur (lambda (y) (make-local-list starting-locals y)))
             (block-recur (lambda (nls body preserved-locals)
                            (let ((locals (collect-locals-in-scope body preserved-locals)))
                              (LexBlock
                               nls
                                  (move-past-local-lets
                                   (LexInScopeLocals (filter-locals locals))
                                   (make-local-list preserved-locals body)))))))

         (type-case LexExpr y
           [LexBlock (nls body)
                     (block-recur nls body nls)]
           [LexClass (scope name bases body keywords stararg kwarg decorators)
                     (LexClass scope name (map recur bases)
                     (type-case LexExpr body
                       [LexBlock (nls es)
                                 (let ((locals (collect-instance-in-scope es nls)))
                                   (LexBlock nls 
                                             (move-past-local-lets
                                              (LexInScopeLocals (filter-locals locals))
                                              (make-local-list nls es))))

                       ]
                       [else (error 'make-local-list
                                    (format "thing inside class body is not block:~a" body))])
                     (map recur keywords)
                     (option-map recur stararg)
                     (option-map recur kwarg)
                     (map recur decorators))]
           [LexTryExceptElse (try except el)
                             (LexTryExceptElse
                              (make-local-list starting-locals try)
                              (map (lambda (y)
                                     (begin
                                       
                                        (move-past-LexExcept
                                         (LexInScopeLocals (filter-locals starting-locals))
                                         (make-local-list starting-locals y))
                                        ;(make-local-list starting-locals y)
                                       )) except)
                              (make-local-list starting-locals el))]
           [LexTryFinally (try finally)
                          (LexTryFinally
                           (make-local-list starting-locals try)
                           (LexSeq (list
                                    (LexInScopeLocals (filter-locals starting-locals))
                                    (make-local-list starting-locals finally)))
                           )]
           [else (default-recur)])))))

