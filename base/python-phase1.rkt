#lang plai-typed

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
 


;I want encapsulation, so I guess this works
(define (new-scope-phase expr)
  (local
   [
         
(define (assert-on-tree [fun : (LexExpr -> boolean)] [message : string] [expr : LexExpr] ) : LexExpr
  (call/cc
   (lambda (outer)
     (begin

       (call/cc
        (lambda (inner)
          (outer (lexexpr-modify-tree expr (lambda (x) (if (fun x) (haiku-error) (inner (display message))))))))
       (error 'assert-on-tree: message)
       ))))

(define (cascade-nonlocal [args : (listof symbol)] [body : LexExpr] ) : LexExpr
  (if (empty? args)
      body
      (LexSeq (list (PyLexNonLocal  args) body))))


(define (pre-desugar [expr : PyExpr]) : LexExpr
  (assert-on-tree (lambda (y) true) "assert-on-tree seems to work" 
  (pyexpr-modify-tree
   expr
   (lambda (y)
     (type-case PyExpr y
       [PyClass (name bases body)
                (LexSeq (list (LexAssign (list (PyLexId name 'Store)) (LexUndefined))
                              (LexClass (Unknown-scope) name bases (LexBlock empty
                                                                             (pre-desugar body)))))]
       [PyLam (args body)
              (LexLam args (LexBlock args (cascade-nonlocal args (pre-desugar body))))]
       [PyFunc (name args defaults body decorators)
               (LexSeq (list (LexAssign (list (PyLexId name 'Store)) 
                                        (LexFunc name args
                                                 (map pre-desugar defaults)
                                                 (LexBlock args (cascade-nonlocal args (pre-desugar body)))
                                                 (map pre-desugar decorators)
                                                 (none)))))]
       
       [PyFuncVarArg (name args sarg body decorators)
                     (LexSeq (list
                              (LexAssign (list (PyLexId name 'Store)) 
                                         (LexFuncVarArg name args sarg
                                                        (LexBlock (cons sarg args)
                                                                  (cascade-nonlocal (cons sarg args) (pre-desugar body)))
                                                        (map pre-desugar decorators)
                                                        (none)))))]
       [else (haiku-error)]
       )))))

(define (introduce-locals listof-locs expr)
        (cond
         [(empty? listof-locs) expr]
         [else (LexLocalLet (first listof-locs) (LexUndefined)
                            (introduce-locals (rest listof-locs) expr))]))

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
             [else (haiku-error)]))))
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
             [LexBlock (nls es) e]
             [LexClass (scope name bases body) e]
             [else (haiku-error)]))))
      (define (generate-identifier )
        (gensym 'class-replacement))
      ]
       hoist-functions))
      ;end hoist functions.
      
      (define (replace-instance expr class-expr)
        (lexexpr-modify-tree
         expr
         (lambda (y)
           (type-case LexExpr y
             [LexInstanceId (x ctx) (LexDotField class-expr x)]
             [LexClass (scope name bases body) (LexClass scope name bases body)]
             [else (haiku-error)]))))
      (define (annotate-methods-with-class expr classname)
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
             [else (haiku-error)]))))
      (define (split-instance-into-instance-locals expr)
        (introduce-locals
         (lexexpr-fold-tree expr (lambda (y)
                                   (type-case LexExpr y
                                     [LexInstanceId (x ctx) (list x)]
                                     [LexClass (scope name bases body) empty]
                                     [LexBlock (nls es) empty]
                                     [else (haiku-error)])))
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
                               [else (haiku-error)])))))
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
               [else (haiku-error)])))))
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
                             (let ((new-body (replace-instance
                                              body
                                              class-expr
                                              )))
                               (LexSeq (list (LexAssign
                                              (list class-expr)
                                              (LexClass scope name bases (LexPass)))
                                             (deal-with-class
                                              (annotate-methods-with-class new-body name)
                                              class-expr))))))]
             [else (haiku-error)])))))
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
                 [else (haiku-error)])))))]
        (top-level-deal-with-class expr)))

(define (scope-phase [expr : PyExpr] ) : LexExpr
  (LexModule
   (list 
    (optimization-pass
     (let-phase
      (post-desugar
       (let ((replaced-locals (replace-all-locals  (replace-all-instance (pre-desugar expr)) empty empty)))
         (let ((fully-transformed (make-all-global replaced-locals))) 
           (remove-blocks
            (remove-unneeded-pypass
             (remove-nonlocal
              (remove-global
               (replace-lexmodule
                (remove-unneeded-assigns
                 (process-syntax-errors
                  (bind-locals fully-transformed)))))))))))))))) ;surround every block with PyLet of locals

(define (replace-lexmodule expr)
  (lexexpr-modify-tree expr
                       (lambda (y)
                         (type-case LexExpr y
                           [LexModule (es ) (LexSeq (map replace-lexmodule es))]
                           [else (haiku-error)]))))


(define (remove-unneeded-assigns expr)
      (lexexpr-modify-tree expr
                           (lambda (y)
                             (type-case LexExpr y
                               [LexSeq
                                (es)
                                (if (and ( = (length es) 2)
                                         (LexAssign? (first es))
                                         (LexUndefined? (LexAssign-value (first es))))
                                    (let ((replace-scope
                                           (cond
                                            [(LexLocalId? (first (LexAssign-targets (first es))))
                                             (Locally-scoped)]
                                            [(LexGlobalId? (first (LexAssign-targets (first es))))
                                             (Globally-scoped)]
                                            [(LexInstanceId? (first (LexAssign-targets (first es))))
                                             (Instance-scoped)]
                                            [else
                                             (error 'remove-unneeded-assigns "assignment is not to ID type")])))
                                      (type-case LexExpr (second es)
                                        [LexClass (scope name bases body)
                                                  (LexClass replace-scope name bases
                                                            (remove-unneeded-assigns body))]
                                        [LexFunc (name args defaults body decorators class)
                                                 (LexFunc name args
                                                          (map remove-unneeded-assigns defaults)
                                                          (remove-unneeded-assigns body)
                                                          (map remove-unneeded-assigns decorators)
                                                          class)]
                                        [LexFuncVarArg (name args sarg body decorators class)
                                                       (LexFuncVarArg name args sarg
                                                                      (remove-unneeded-assigns body)
                                                                      (map remove-unneeded-assigns decorators)
                                                                      class)]
                                        [else (error 'remove-unneeded-assigns
                                                     "undefined pattern present without declaration")]
                                        )
                                      )
                                    (LexSeq (map remove-unneeded-assigns es)))]
                               
                               [else (haiku-error)]))))
(define (optimization-pass expr)
  expr)


(define (process-syntax-errors [expr : LexExpr]) : LexExpr
  (call/cc
   (lambda (k)
     (local
      [(define (bindings-for-nonlocal [bindings : (listof symbol)] [expr : LexExpr]) : LexExpr
         (let ((these-locals empty))
           (lexexpr-modify-tree
               expr
               (lambda (e)
                 (type-case LexExpr e
                   [PyLexNonLocal (locals) (if
                                            (empty? (list-subtract locals bindings))
                                            (PyLexNonLocal locals)
                                            (k 
                                             (LexModule
                                              (list (LexRaise
                                                     (LexApp
                                                      (LexGlobalId 'SyntaxError 'Load)
                                                      (list (LexStr
                                                             (string-append
                                                              "no binding for nonlocal '"
                                                              (string-append
                                                               (symbol->string (first locals))
                                                               "' found"))))))))))]
                                  [LexBlock (nls e) (LexBlock nls (bindings-for-nonlocal
                                                                   (remove-duplicates
                                                                    (flatten (list bindings these-locals nls)))
                                                                   e))]
                                  [LexLocalId (x ctx) (begin (set! these-locals (cons x these-locals)) e)]
                                  [else (haiku-error)]
                                  )))))]
       (k (bindings-for-nonlocal empty expr))))))

(define (let-phase [expr : LexExpr] ) : LexExpr
(collapse-pyseq (cascade-undefined-globals (list-subtract
                                            (begin
                                              ;(display (extract-post-transform-globals expr))
                                              (extract-post-transform-globals expr))
                                            library-names) expr))) ;all globals, not just the current scope

(define library-names (map (lambda (b) (bind-left b)) lib-function-dummies))

(define (collapse-pyseq expr ) 
  (lexexpr-modify-tree
   expr 
   (lambda (x) (type-case LexExpr x
                 [LexSeq(lis)
                        (LexSeq (flatten (map (lambda (y)
                                                (let ((e (collapse-pyseq y)))
                                                  (if (LexSeq? e) (LexSeq-es e) (list e))))
                                              lis)))]
                 [else (haiku-error)]))))
(define (remove-global expr)
  (lexexpr-modify-tree
   expr
   (lambda (x)
     (type-case LexExpr x
       [PyLexGlobal(y) (LexPass)]
       [else (haiku-error)]))))

(define (remove-nonlocal expr)
  (lexexpr-modify-tree
   expr
   (lambda (x)
     (type-case LexExpr x
       [PyLexNonLocal(y) (LexPass)]
       [else (haiku-error)]))))

(define (remove-unneeded-pypass expr)
  (lexexpr-modify-tree
   expr
   (lambda (x)
     (type-case LexExpr x
       [LexSeq(y)
             (let ((filtered-seq (filter (lambda (z) (not (LexPass? z))) y)))
               (cond
                [(empty? filtered-seq) (LexPass)]
                [(empty? (rest filtered-seq)) (remove-unneeded-pypass (first filtered-seq))]
                [else (LexSeq (map remove-unneeded-pypass filtered-seq))]))]
       [else (haiku-error)]))))

(define (remove-blocks expr)
  (lexexpr-modify-tree
   expr
   (lambda (x)
     (type-case LexExpr x
       [LexBlock (nls e) (remove-blocks e)]
       [else (haiku-error)]))))



#;(define (assert-pyblock-exists expr)
  (if (empty? (lexexpr-fold-tree expr (lambda (x) (type-case LexExpr x
                                             [LexBlock(_) true]
                                             [else false]))
                                (lambda (x) (type-case LexExpr x
                                        [LexBlock(a) (list a)]
                                        [else (haiku-error)]))))
      (error 'assert-pyblock-exists "pyblock is missing")
      expr))
(define (cascade-undefined-globals [globals : (listof symbol) ] [body : LexExpr] ) : LexExpr
  (if (empty? globals)
      body
      (LexGlobalLet (first globals) (LexUndefined)
            (cascade-undefined-globals (rest globals) body))))

(define (cascade-undefined-locals [globals : (listof symbol) ] [body : LexExpr] ) : LexExpr
  (if (empty? globals)
      body
      (LexLocalLet (first globals) (LexUndefined)
            (cascade-undefined-locals (rest globals) body))))

;(define (cascade-let [ids : (listof symbol) ] [ expr : (listof CExp) ] [body : CExp] ) : CExp
;  (cond
;   [(empty? ids) body]
;   [(empty? expr) body]
;   [else (CLet (first ids) (first expr) (cascade-let (rest ids) (rest expr) body))]))

(define (extract-post-transform-globals expr) : (listof symbol)
  (remove-duplicates
   (lexexpr-fold-tree
    expr
    (lambda (exp)
      (type-case LexExpr exp
        [LexGlobalId (x ctx) (list x)]
        #;[LexAssign (lhs rhs) (map (lambda (y) (LexGlobalId-x y))
                                    (filter (lambda (y) (LexGlobalId? y)) lhs))]
        [else (haiku-error)])))))


;remember, (extract-globals expr true) is _shallow_ 
(define (extract-globals [expr : LexExpr] [current-scope-only? : boolean] ) : (listof symbol)
  (let 
      ((extract-globals-cls (lambda (x) (extract-globals x current-scope-only?))))
    (lexexpr-fold-tree expr 
                      (lambda (exp)
                        (type-case LexExpr exp
                          [LexBlock (nonlocals es) (if current-scope-only? empty (extract-globals-cls es))]
                          [PyLexGlobal(globals) globals]
                          [else (haiku-error)])))))
(define (find-all-instance expr)
      (let [[post-remove
      (list-subtract
       (list-subtract
        (lexexpr-fold-tree
         expr
         (lambda (x) : (listof symbol)
           (type-case LexExpr x
             [LexAssign (lhs rhs) (map (lambda (y)
                                         (type-case LexExpr y
                                          [PyLexId (name ctx) name]
                                          [else (error 'find-all-instance "can't handle non-pyid")])) lhs)]
             [LexAugAssign (op lhs rhs) (list (PyLexId-x lhs))]
             [LexBlock (_ __) empty]
             [else (haiku-error)])))
        (extract-globals expr true))
         (extract-nonlocals expr))]]
        post-remove))

(define (extract-unreplaced-locals [expr : LexExpr ] ) : (listof symbol)
  (let ((overestimate (filter (lambda (x) (not (contains-char? (symbol->string x) (chr "-") )))
           (remove-duplicates (extract-locals-helper expr))))
        (globals (extract-globals expr true)))
    (begin
;      (display "expr: ")
;      (display expr)
;      (display "\n\n")
;      (display overestimate)
;      (display "\n\n")
;      (display globals)
;      (display "\n\n")
      (let ((result (list-subtract overestimate globals)))
        (begin
;          (display result)
;          (display "\n\n END END END\n\n")
          result))
      
    )))


;takes a tree to traverse (the expression)
(define (extract-locals-helper [expr : LexExpr] ) : (listof symbol)
  (letrec ((target-fun
         (lambda (exp)
           (type-case LexExpr exp
             [PyLexId (sym ctx) (list sym)]
             [LexInstanceId (_ __) empty]
             [else (haiku-error)])))
        (spec (lambda (exp)
                (type-case LexExpr exp
                  [LexAssign (targets value)
                             (let
                                 ((target-ids (flatten (map target-fun targets))))
                               (flatten (list (extract-locals-helper value) target-ids)))]
                  [LexAugAssign (op target value) (flatten (list (extract-locals-helper value)
                                                                (target-fun target))) ]
                  [PyLexNonLocal (ids) ids]
                  [LexBlock (nls es) empty]
                  [LexExceptAs (types name body) (list name)]
                  [else (haiku-error)]))))
    (lexexpr-fold-tree expr spec)))

(define (replace-all-instance [expr : LexExpr]) : LexExpr
  (local
   [
    (define (toplevel [expr : LexExpr ]) : LexExpr
      (lexexpr-modify-tree
       expr
       (lambda (x)
         (type-case LexExpr x
           [LexClass (scope name bases body)
                           (LexClass
                            scope
                            name
                            bases
                            (type-case LexExpr body
                              [LexBlock (nls e)
                                       (LexBlock
                                        nls
                                        (second-level e))]
                              [else (begin (display "thing in class literal is not a block")
                                           (error 'e "thing in class literal is not block"))]))]
           [else (haiku-error)]))))
    (define (second-level expr )
      (let
          ((locs (find-all-instance expr)))
        (lexexpr-modify-tree
         expr
         (lambda (x)
           (let ((assign-func  (lambda (x) : LexExpr
                                 (type-case LexExpr x
                                   [PyLexId (y ctx) (if
                                               (member y locs)
                                               (LexInstanceId y ctx)
                                               x)]
                                   [else x]))))
             (type-case LexExpr x
                                        ;[LexId (e) (LexInstanceId e)]
               [LexAssign (targets value) (LexAssign (map assign-func
                                                      targets) value)]
               [LexAugAssign (op target value) (LexAugAssign op (assign-func target) value)]
               [LexBlock (nls e) (LexBlock nls (toplevel e))]
               [LexClass (scope name super body) (toplevel x)]
               [else (haiku-error)]))))))
    ]
   (toplevel expr)))
      
(define (all-replaced-instance [es : LexExpr])
  (lexexpr-fold-tree
   es
   (lambda ( [e : LexExpr] )
     (type-case LexExpr e
       [LexInstanceId (x ctx) (list x)]
       [LexBlock (_ __) empty]
       [else (haiku-error)]))))

(define (replace-all-locals [expr : LexExpr] [locs : (listof symbol) ] [instance : (listof symbol)])
  (let ((replace (lambda ([str : symbol] [ctx : symbol]) (if (empty? (flatten
                                                                      (list
                                                                       (filter (lambda (x) (equal? str x)) locs)
                                                                       (filter (lambda (x) (equal? str x)) instance))))
                                             (PyLexId str ctx)
                                             (if (contains-char? (symbol->string str) (chr "-"))
                                                 (error 'replace-all-locals
                                                        (format
                                                         "has dash: ~a" str))
                                                 (LexLocalId str ctx)))))
        (recur (lambda ([this-expr : LexExpr]) (replace-all-locals this-expr locs instance)))
        )
    (lexexpr-modify-tree expr
     (lambda (exp)
       (type-case LexExpr exp
         [PyLexId (x ctx)  (replace x ctx)]
         [LexBlock (nls es)
                   (LexBlock nls
                             (replace-all-locals es
                                                 (remove-duplicates
                                                  (flatten (list
                                                            (list-subtract locs (extract-globals es true))
                                                            (extract-unreplaced-locals es))))
                                                 ;all (replaced) instance variables in this scope.
                                                 (all-replaced-instance es)
                                                      ))]         
         [else (haiku-error)])))))

(define (make-all-global [expr : LexExpr]) : LexExpr
;  (lexexpr-modify-tree
  (assert-on-tree
   (lambda (y) (not (PyLexId? y))) "make-all-global failed: PyLexId present"
   (begin
     ;(display "recurring into make-all-global\n")
     (lexexpr-modify-tree
      expr
      (lambda (x)
        (type-case LexExpr x
          [PyLexId (x ctx) (begin #;(display "it's an ID\n") (LexGlobalId x ctx))]
                                        ;[LexReturn (r) (LexReturn (make-all-global r))]
          [else (begin #;(display "executing default\n") (haiku-error))]))))
      ))

;finds all PyLexNonLocal designators in this block
(define (extract-nonlocals [expr : LexExpr] ) : (listof symbol)
  (lexexpr-fold-tree expr
                    (lambda (x)
                      (type-case LexExpr x
                        [LexBlock (nls e) empty]
                        [PyLexNonLocal (l) l]
                        [else (haiku-error)]))))


;only binds renamed locals in this scope.
(define (bind-locals [ expr : LexExpr]) : LexExpr
  (local
   [
    ;this extracts all the things that should be let bound (in this scope).
    ;that's all the identifiers that are assigned to which aren't nonlocal,
    ;and not counting descendent blocks.
    (define (extract-locals [expr : LexExpr ] ) : (listof symbol)
      (list-subtract
       (list-subtract
        (filter (lambda (x) (not (contains-char? (symbol->string x) (chr "-") )))
                (remove-duplicates (extract-locals-helper expr)))
        (extract-globals expr true))
       (extract-nonlocals expr)
       ))
    
    
;takes a tree to traverse (the expression)
    (define (extract-locals-helper [expr : LexExpr] ) : (listof symbol)
      (letrec ((targ-fun
             (lambda (exp) : (listof symbol)
               (type-case LexExpr exp
                 [LexLocalId (sym ctx) (list sym)]
                 [LexGlobalId (_ __) empty]
                 [LexInstanceId (_ __) empty]
                 [else (haiku-error)]
                 )))
            (spec (lambda (exp) (type-case LexExpr exp
                 [LexAssign (targets value)
                           (let
                               ((target-ids (flatten (map targ-fun
                                                 targets))))
                             (flatten (list (extract-locals-helper value) target-ids)))]
                 [LexAugAssign (op l r) (flatten (list (extract-locals-helper r) (targ-fun l)))]
                 [LexBlock (nls es) empty]
                 [else (haiku-error)]))))
        (lexexpr-fold-tree expr spec)))]
   
   (lexexpr-modify-tree
    expr
    (lambda (x)
      (type-case LexExpr x
        [LexBlock (nl es) (LexBlock nl (cascade-undefined-locals
                                (let ((found-locals (extract-locals es)))
                                  (remove-duplicates found-locals))
                                (bind-locals es )))]
        [else (haiku-error)])))))

]
(scope-phase expr)))
