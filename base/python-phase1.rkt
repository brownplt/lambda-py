#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt"
         "python-lexical-syntax.rkt"
         "python-primitives.rkt"
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

(define (pre-desugar [expr : PyExpr]) : LexExpr
  (assert-on-tree (lambda (y) true) "assert-on-tree seems to work" 
  (pyexpr-modify-tree
   expr
   (lambda (y)
     (type-case PyExpr y
       [PyClass (name bases body) (LexClass name bases (LexBlock empty (pre-desugar body)))]
       [PyLam (args body) (LexLam args (LexBlock args (cascade-nonlocal args (pre-desugar body))))]
       [PyFunc (name args defaults body decorators) (LexFunc name args (map pre-desugar defaults)
                                                             (LexBlock args (cascade-nonlocal args (pre-desugar body)))
                                                             (map pre-desugar decorators))]
       [PyFuncVarArg (name args sarg body decorators) (LexFuncVarArg name args sarg
                                                                     (LexBlock args (cascade-nonlocal args (pre-desugar body)))
                                                                     (map pre-desugar decorators))]
       [else (haiku-error)]
       )))))

(define (scope-phase [expr : PyExpr] ) : LexExpr
  (LexModule
   (list 
    (optimization-pass
     (let-phase
      (let ((replaced-locals (replace-all-locals  (replace-all-instance (pre-desugar expr)) empty))) ;replaces all locals everywhere based on assignment
        (let ((fully-transformed (make-all-global replaced-locals))) ;replaces all remaining PyId 
          (remove-blocks
           (remove-unneeded-pypass
            (remove-nonlocal
             (remove-global
              (replace-lexmodule
               (process-syntax-errors
                (bind-locals fully-transformed)))))))))))))) ;surround every block with PyLet of locals

(define (replace-lexmodule expr)
  (lexexpr-modify-tree expr
                       (lambda (y)
                         (type-case LexExpr y
                           [LexModule (es ) (LexSeq (map replace-lexmodule es))]
                           [else (haiku-error)]))))

(define (optimization-pass expr) expr)

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
                                            (k (LexModule
                                                (list (LexRaise
                                                       (LexApp
                                                        (LexGlobalId 'SyntaxError 'Load)
                                                        (list (LexStr (string-append "no binding for nonlocal '"
                                                                                     (string-append (symbol->string (first locals)) "' found"))))))))))]
                   [LexBlock (nls e) (LexBlock nls (bindings-for-nonlocal
                                                    (remove-duplicates (flatten (list bindings these-locals nls)))
                                                    e))]
                   [LexLocalId (x ctx) (begin (set! these-locals (cons x these-locals)) e)]
                   [else (haiku-error)]
                   )))))]
    (k (bindings-for-nonlocal empty expr))))))

(define (let-phase [expr : LexExpr] ) : LexExpr
(collapse-pyseq (cascade-undefined-globals (extract-post-transform-globals expr) expr))) ;all globals, not just the current scope

(define (collapse-pyseq expr ) 
  (lexexpr-modify-tree expr 
                       (lambda (x) (type-case LexExpr x
                                     [LexSeq(lis) (LexSeq (flatten (map (lambda (y) (if (LexSeq? y) (LexSeq-es y) (list y)))lis)))]
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
        ;[LexGlobalId (x ctx) (list x)]
        [LexAssign (lhs rhs) (filter (lambda (y) (LexGlobalId? y)) lhs )]
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
  (list-subtract
   (filter (lambda (x) (not (contains-char? (symbol->string x) (chr "-") )))
           (remove-duplicates (extract-locals-helper expr)))
   (extract-globals expr true)))


;takes a tree to traverse (the expression)
(define (extract-locals-helper [expr : LexExpr] ) : (listof symbol)
  (letrec ((target-fun
         (lambda (exp)
           (type-case LexExpr exp
             [PyLexId (sym ctx) (list sym)]
             [LexInstanceId (_ __) empty]
             ;[LexGetField (obj _) (extract-locals-helper obj)]
             ;[LexBracketsSet (val sli)
             ;                (flatten
             ;                 (list
             ;                  (extract-locals-helper
             ;                   val)
             ;                  (type-case Slice sli
             ;                    [index (val) (extract-locals-helper val)]
             ;                    [else (error 'e3 "slice not done")])))]
             ;[else (error 'e1 "don't support assigning non-pyid TODO")])))
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
           [LexClass (name bases body)
                           (LexClass
                            name
                            bases
                            (type-case LexExpr body
                              [LexBlock (nls e)
                                       (LexBlock
                                        nls
                                        (second-level e))]
                              [else (begin (display "thing in class literal is not a block") (error 'e "thing in class literal is not block"))]))]
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
               [LexClass (name super body) (toplevel x)]
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
                                                        (format "it appears we have an old-style variable name we have encountered: ~a" str))
                                                 (LexLocalId str ctx)))))
        (recur (lambda ([this-expr : LexExpr]) (replace-all-locals this-expr locs))))
    (lexexpr-modify-tree expr
     (lambda (exp)
       (type-case LexExpr exp
         [PyLexId (x ctx)  (replace x ctx)]
         [LexBlock (nls es) (LexBlock nls (replace-all-locals es (remove-duplicates (flatten (list locs (extract-unreplaced-locals es))))))]
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
    
    
                                        ;takes a tree to traverse (the expression) and env, which is a list of locals which have been discovered in parent scopes.
    (define (extract-locals-helper [expr : LexExpr] ) : (listof symbol)
      (letrec ((targ-fun
             (lambda (exp) : (listof symbol)
               (type-case LexExpr exp
                 [LexLocalId (sym ctx) (list sym)]
                 [LexGlobalId (_ __) empty]
                 [LexInstanceId (_ __) empty]
                 [else (haiku-error)]
                 ;[LexGetField (obj _) (extract-locals-helper obj)]
                 ;[LexBracketsSet (val sli)
                 ;                (flatten
                 ;                 (list
                 ;                  (extract-locals-helper
                 ;                   val)
                 ;                  (type-case Slice sli
                 ;                    [index (val) (extract-locals-helper val)]
                 ;                    [else (error 'e3 "slice not done")])))]
                 ;[else (error 'e2 "don't support assigning non-pyid TODO")]
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

;I'm thinking this is all hopelessly inefficient, but whatever.
;this is only in the current scope, only finds vars that start with "local-"
#;(define (find-vars-with-prefix [expr : LexExpr] [prefix : string] ) : (listof symbol)
  (lexexpr-fold-tree expr
                    (lambda (exp) (type-case LexExpr exp
                                    [LexBlock (es) true]
                                    [LexAssign (t v) true]
                                    [else false]))
                    (lambda (exp) (type-case LexExpr exp
                                    [LexBlock (es) empty]
                                    [LexAssign (targ v)
                                              (cond
                                               [(not (empty? (rest targ))) (error 'e "no multiple assignment, sorry")]
                                               [else 
                                                (type-case LexExpr (first targ)
                                                  [LexId (target)
                                                        (flatten (list 
                                                                  (filter (lambda (y) (contains-string? prefix (symbol->string y) )) (list target))
                                                                  (find-vars-with-prefix v prefix)))]
                                                  [else (error 'e "can't assign non py-ids yet")])])]
                                    [else (error 'desugar:find-renamed-locals "shouldn't get here")]))))



#|(test
 (extract-globals (LexSeq (list 
			(LexFunctionDef 
			 'f (list ) (list)
			 (LexBlock (LexSeq (list 
							   (PyLexGlobal(list 'x)) 
							   (LexAssign (list (LexId 'x)) (LexNum 12)) 
							   (LexApp (LexId 'print) (list (LexId 'local-x))) 
							   (LexId 'x)))) (list) (none)) 
			(LexApp (LexId 'f) (list)) 
			(LexApp (LexId 'print) (list (LexId 'x)))))false)
(list 'x)
 
 )
(test
 (extract-unreplaced-locals (LexSeq (list 
			(LexFunctionDef 
			 'f (list ) (list)
			 (LexBlock 
					   (LexSeq (list 
							   (PyLexGlobal(list 'x)) 
							   (LexAssign  (list (LexId 'x)) (LexNum 12)) 
							   (LexApp (LexId 'print) (list (LexId 'local-x))) 
							   (LexId 'x)))) (list) (none)) 
			(LexApp (LexId 'f) (list)) 
			(LexApp (LexId 'print) (list (LexId 'x))))))(list))

(test
 (extract-unreplaced-locals 
  (LexSeq (list 
          (PyLexGlobal(list 'x)) 
          (LexAssign  (list (LexId 'x)) (LexNum 12)) 
          (LexApp (LexId 'print) (list (LexId 'local-x))) 
          (LexId 'x))))
 (list))
  
(test
 (extract-globals 
  (LexSeq (list 
          (PyLexGlobal(list 'x)) 
          (LexAssign  (list (LexId 'x)) (LexNum 12)) 
          (LexApp (LexId 'print) (list (LexId 'local-x))) 
          (LexId 'x))) true)
 
 (list 'x))
  
|#

]
(scope-phase expr)))
