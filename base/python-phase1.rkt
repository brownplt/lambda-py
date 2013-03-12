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
 


;I want encapsulation, so I guess this works
(define (scope-phase expr)
  (local
   [
         
(define (assert-on-tree [fun : (LexExpr -> boolean)] [message : string] [expr : LexExpr] ) : LexExpr
  (call/cc
   (lambda (outer)
     (begin

       (call/cc
        (lambda (inner)
          (outer (lexexpr-modify-tree expr (lambda (x) (if (fun x) (default-recur) (inner (display message))))))))
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
                              (LexClass (Unknown-scope) name
                                        (let ((desugared-bases (map pre-desugar bases)))
                                          (LexTuple (if (empty? desugared-bases)
                                                        (list (LexGlobalId '%object 'Load))
                                                        desugared-bases)))
                                        (LexBlock empty
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
       [PyImport (names asnames) 
                 (desugar-pyimport names asnames)]
       
       [PyImportFrom (module names asnames level)
                     (desugar-pyimportfrom module names asnames level)]
       
       [else (default-recur)]
       )))))

;;; desugar import name as asname to
;;; asname = __import__('name')
;;; desugar-import-py will 
(define (desugar-pyimport names asnames) : LexExpr
  (local [(define (desugar-pyimport/rec names asnames)
            (cond [(empty? names) (list)]
                  [else
                   (append
                    (list (LexAssign
                           (list (PyLexId (first asnames) 'Store))
                           (LexApp (PyLexId '__import__ 'Load)
                                  (list (LexStr (first names))))))
                    (desugar-pyimport/rec (rest names) (rest asnames))
                    )]))]
         (LexSeq (desugar-pyimport/rec names asnames))))



;;; desugar from module import name as asname
;;; __tmp_module = __import__(module, globals(), locals(), [id], 0)
;;; id_alias = __tmp_module.id
;;; 
;;;  from module import * is not available now
(define (desugar-pyimportfrom [module : string]
                              [names : (listof string)]
                              [asnames : (listof symbol)]
                              [level : number]) : LexExpr
  (local [(define tmp-module (PyLexId '$__tmp_module 'Store))
          (define module-expr
            (LexAssign (list tmp-module)
                      (LexApp (PyLexId '__import__ 'Load)
                              (list (LexStr module)))))]
    (cond [(not (equal? (first names) "*"))
           (local [(define (get-bind-exprs module names asnames)
                     (cond [(empty? names) (list)]
                           [else
                            (append (list (LexAssign (list (PyLexId (first asnames) 'Store))
                                                    (LexDotField module (string->symbol (first names)))))
                                    (get-bind-exprs module (rest names) (rest asnames)))]))
                   (define bind-exprs
                     (get-bind-exprs tmp-module names asnames))]
             (LexSeq (append (list module-expr) bind-exprs)))]
          [else ;; from module import *
           (error 'desugar "star expression is not allowed yet.")])))

 
(define (scope-phase [expr : PyExpr] ) : LexExpr
       (let ((replaced-locals (replace-all-locals  (replace-all-instance (pre-desugar expr)) empty empty)))
         (let ((fully-transformed (make-all-global replaced-locals))) 
           (remove-blocks
            (remove-unneeded-pypass
             (remove-nonlocal
              (remove-global
               (replace-lexmodule
                (remove-unneeded-assigns
                 (process-syntax-errors
                  (bind-locals fully-transformed))))))))))) ;surround every block with PyLet of locals

(define (replace-lexmodule expr)
  (lexexpr-modify-tree expr
                       (lambda (y)
                         (type-case LexExpr y
                           [LexModule (es ) (LexSeq (map replace-lexmodule es))]
                           [else (default-recur)]))))


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
                                                  (LexClass replace-scope name
                                                            (remove-unneeded-assigns bases)
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
                               
                               [else (default-recur)]))))



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
                                  [else (default-recur)]
                                  )))))]
       (k (bindings-for-nonlocal empty expr))))))




(define (remove-global expr)
  (lexexpr-modify-tree
   expr
   (lambda (x)
     (type-case LexExpr x
       [PyLexGlobal(y) (LexPass)]
       [else (default-recur)]))))

(define (remove-nonlocal expr)
  (lexexpr-modify-tree
   expr
   (lambda (x)
     (type-case LexExpr x
       [PyLexNonLocal(y) (LexPass)]
       [else (default-recur)]))))

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
       [else (default-recur)]))))

(define (remove-blocks expr)
  (lexexpr-modify-tree
   expr
   (lambda (x)
     (type-case LexExpr x
       [LexBlock (nls e) (remove-blocks e)]
       [else (default-recur)]))))



#;(define (assert-pyblock-exists expr)
  (if (empty? (lexexpr-fold-tree expr (lambda (x) (type-case LexExpr x
                                             [LexBlock(_) true]
                                             [else false]))
                                (lambda (x) (type-case LexExpr x
                                        [LexBlock(a) (list a)]
                                        [else (default-recur)]))))
      (error 'assert-pyblock-exists "pyblock is missing")
      expr))

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



;remember, (extract-globals expr true) is _shallow_ 
(define (extract-globals [expr : LexExpr] [current-scope-only? : boolean] ) : (listof symbol)
  (let 
      ((extract-globals-cls (lambda (x) (extract-globals x current-scope-only?))))
    (lexexpr-fold-tree expr 
                      (lambda (exp)
                        (type-case LexExpr exp
                          [LexBlock (nonlocals es) (if current-scope-only? empty (extract-globals-cls es))]
                          [PyLexGlobal(globals) globals]
                          [else (default-recur)])))))
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
             [else (default-recur)])))
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
             [else (default-recur)])))
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
                  [else (default-recur)]))))
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
                            (toplevel bases)
                            (type-case LexExpr body
                              [LexBlock (nls e)
                                       (LexBlock
                                        nls
                                        (second-level e))]
                              [else (begin (display "thing in class literal is not a block")
                                           (error 'e "thing in class literal is not block"))]))]
           [else (default-recur)]))))
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
               [else (default-recur)]))))))
    ]
   (toplevel expr)))
      
(define (all-replaced-instance [es : LexExpr])
  (lexexpr-fold-tree
   es
   (lambda ( [e : LexExpr] )
     (type-case LexExpr e
       [LexInstanceId (x ctx) (list x)]
       [LexBlock (_ __) empty]
       [else (default-recur)]))))

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
         [else (default-recur)])))))

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
          [else (begin #;(display "executing default\n") (default-recur))]))))
      ))

;finds all PyLexNonLocal designators in this block
(define (extract-nonlocals [expr : LexExpr] ) : (listof symbol)
  (lexexpr-fold-tree expr
                    (lambda (x)
                      (type-case LexExpr x
                        [LexBlock (nls e) empty]
                        [PyLexNonLocal (l) l]
                        [else (default-recur)]))))


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
                 [else (default-recur)]
                 )))
            (spec (lambda (exp) (type-case LexExpr exp
                 [LexAssign (targets value)
                           (let
                               ((target-ids (flatten (map targ-fun
                                                 targets))))
                             (flatten (list (extract-locals-helper value) target-ids)))]
                 [LexAugAssign (op l r) (flatten (list (extract-locals-helper r) (targ-fun l)))]
                 [LexBlock (nls es) empty]
                 [else (default-recur)]))))
        (lexexpr-fold-tree expr spec)))]
   
   (lexexpr-modify-tree
    expr
    (lambda (x)
      (type-case LexExpr x
        [LexBlock (nl es) (LexBlock nl (cascade-undefined-locals
                                (let ((found-locals (extract-locals es)))
                                  (remove-duplicates found-locals))
                                (bind-locals es )))]
        [else (default-recur)])))))

]
(scope-phase expr)))
