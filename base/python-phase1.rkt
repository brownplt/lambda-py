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

;adds the "init" function to classes.
;(define (classes-add-init [body : LexExpr] ) body)
(define (classes-add-init [body : LexExpr] )
  (LexSeq
   (list
    (LexAssign (list (PyLexId '__init__ 'Store))
                     (LexLam (list 'self)
                             (LexAssign (list (LexDotField (LexLocalId 'self 'Load) '__class__ ))
                                        (LexBuiltinPrim '$class (list (LexLocalId 'self 'Load))))))
    body)))


(define (pre-desugar [expr : PyExpr]) : LexExpr
  (assert-on-tree (lambda (y) true) "assert-on-tree seems to work" 
  (pyexpr-modify-tree
   expr
   (lambda (y)
     (type-case PyExpr y
       [PyClass (name bases body)
                (LexSeq (list (LexAssign (list (PyLexId name 'Store)) (LexUndefined))
                              (LexClass (Unknown-scope) name bases (LexBlock empty
                                                                             (classes-add-init
                                                                              (pre-desugar body))))))]
       [PyLam (args body)
              (LexLam args (LexBlock args (cascade-nonlocal args (pre-desugar body))))]
       [PyFunc (name args defaults body)
               (LexSeq (list (LexAssign (list (PyLexId name 'Store)) 
                                        (LexFunc name args
                                                 (map pre-desugar defaults)
                                                 (LexBlock args (cascade-nonlocal args (pre-desugar body)))))))]
       [PyClassFunc (name args body)
                    (LexSeq (list
                             (LexAssign (list (PyLexId name 'Store)) 
                                        (LexClassFunc name args
                                                      (LexBlock args
                                                                (cascade-nonlocal args (pre-desugar body)))))))]
       [PyFuncVarArg (name args sarg body)
                     (LexSeq (list
                              (LexAssign (list (PyLexId name 'Store)) 
                                         (LexFuncVarArg name args sarg
                                                        (LexBlock args
                                                                  (cascade-nonlocal args (pre-desugar body)))))))]
       [else (haiku-error)]
       )))))

(define (post-desugar [expr : LexExpr]) : LexExpr
  (local
   [(define (replace-instance expr class-expr)
      (lexexpr-modify-tree
       expr
       (lambda (y)
         (type-case LexExpr y
           [LexInstanceId (x ctx) (LexDotField class-expr x)]
           [LexClass (scope name bases body) (LexClass scope name bases body)]
           [else (haiku-error)]))))
    (define (deal-with-class expr class-expr)
      (lexexpr-modify-tree
       expr
        (lambda (y)
          (type-case LexExpr y
            [LexClass (scope name bases body)
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
                                        (deal-with-class new-body class-expr)))))]
            [else (haiku-error)]))))
    (define (top-level-deal-with-class expr)
      (lexexpr-modify-tree
       expr
       (lambda (y)
         (type-case LexExpr y
           [LexClass (scope name bases body) (begin
                                               (if (Instance-scoped? scope)
                                                 (error 'lexical "instance is not inside class")
                                                 (deal-with-class y
                                                                  (if (Globally-scoped? scope)
                                                                      (LexGlobalId name 'Load)
                                                                      (LexLocalId name 'Load)))))]
           [else (haiku-error)]))))]
   (top-level-deal-with-class expr)))

(define (scope-phase [expr : PyExpr] ) : LexExpr
  (LexModule
   (list 
    (optimization-pass
     (let-phase
      (post-desugar
       (let ((replaced-locals (replace-all-locals  (replace-all-instance (pre-desugar expr)) empty)))
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
                                        [LexFunc (name args defaults body)
                                                 (LexFunc name args
                                                          (map remove-unneeded-assigns defaults)
                                                          (remove-unneeded-assigns body))]
                                        [LexClassFunc (name args body)
                                                      (LexClassFunc name args
                                                                    (remove-unneeded-assigns body))]
                                        [LexFuncVarArg (name args sarg body)
                                                       (LexFuncVarArg name args sarg
                                                                      (remove-unneeded-assigns body))]
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
  (lexexpr-modify-tree expr 
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
        [else (error 'desugar:extract-all "we should not get here")])))))


;remember, (extract-globals expr true) is _shallow_ 
(define (extract-globals [expr : LexExpr] [current-scope-only? : boolean] ) : (listof symbol)
  (let 
      ((extract-globals-cls (lambda (x) (extract-globals x current-scope-only?))))
    (lexexpr-fold-tree expr 
                      (lambda (exp)
                        (type-case LexExpr exp
                          [LexBlock (nonlocals es) (if current-scope-only? empty (extract-globals-cls es))]
                          [PyLexGlobal(globals) globals]
                          [else (error 'desugar:extract-globals "should never get here")])))))


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
                  [else (error 'desugar:extract-locals-helper "this shouldn't be reachable")]))))
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
        post-remove))]
   (toplevel expr)))
      

(define (replace-all-locals [expr : LexExpr] [locs : (listof symbol) ])
  (let ((replace (lambda ([str : symbol] [ctx : symbol]) (if (empty? (filter (lambda (x) (equal? str x)) locs))
                                             (PyLexId str ctx)
                                             (if (contains-char? (symbol->string str) (chr "-"))
                                                 (error 'replace-all-locals
                                                        (format
                                                         "has dash: ~a" str))
                                                 (LexLocalId str ctx)))))
        (recur (lambda ([this-expr : LexExpr]) (replace-all-locals this-expr locs))))
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
                                                            (extract-unreplaced-locals es))))))]
         [else (error 'desugar:rename-locals "we should not get here")])))))

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
                 [else (error 'desugar:extract-locals-helper "this shouldn't be reachable")]))))
        (lexexpr-fold-tree expr spec)))]
   
   (lexexpr-modify-tree
    expr
    (lambda (x)
      (type-case LexExpr x
        [LexBlock (nl es) (LexBlock nl (cascade-undefined-locals
                                (let ((found-locals (extract-locals es)))
                                  (remove-duplicates found-locals))
                                (bind-locals es )))]
        [else (error 'desugar:bind-locals "not here")])))))

]
(scope-phase expr)))
