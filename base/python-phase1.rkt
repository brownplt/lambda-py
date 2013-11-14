#lang plai-typed/untyped

(require "python-syntax.rkt"
         "python-core-syntax.rkt"
         "python-lib-bindings.rkt"
         "python-lexical-syntax.rkt"
         "python-lexical-printer.rkt"
         "modules/builtin-modules.rkt"
         "python-syntax-operations.rkt")
(require "util.rkt")
(require [typed-in racket (format : (string 'a -> string))])
(require [typed-in racket (member : ('a (listof 'a) -> boolean))])
(require [typed-in racket (flatten : ((listof (listof 'a) ) -> (listof 'b)))])
(require [typed-in racket (remove-duplicates : ((listof 'a) -> (listof 'a)))])
(require [typed-in racket (gensym : (symbol -> symbol))])
(require [typed-in racket (substring : (string number number -> string))])
(require [typed-in racket (string-length : (string -> number))])
(require [typed-in racket (pretty-write : ('a -> void))])


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
       [PyClass (name bases body keywords stararg kwarg decorators)
                (LexSeq (list (LexAssign (list (PyLexId name 'Store)) (LexPass))
                              (LexClass (Unknown-scope) name
                                        (map pre-desugar bases)
                                        (LexBlock empty
                                                  (pre-desugar body))
                                        (map pre-desugar keywords)
                                        (option-map pre-desugar stararg)
                                        (option-map pre-desugar kwarg)
                                        (map pre-desugar decorators))))]
       [PyLam (args vararg kwonlyargs kwarg defaults kw_defaults body)
              (let ([all-args (flatten (list args (option->list vararg) kwonlyargs (option->list kwarg)))])
                (LexLam args vararg kwonlyargs kwarg (map pre-desugar defaults) (map pre-desugar kw_defaults)
                        (LexBlock all-args (cascade-nonlocal all-args (pre-desugar body)))))]
       [PyFunc (name args vararg kwonlyargs kwarg defaults kw_defaults body decorators)
               (let ([all-args (flatten (list args (option->list vararg) kwonlyargs (option->list kwarg)))])
                 (LexSeq (list (LexAssign (list (PyLexId name 'Store)) 
                                          (LexFunc name args vararg kwonlyargs kwarg
                                                   (map pre-desugar defaults)
                                                   (map pre-desugar kw_defaults)
                                                   (LexBlock all-args
                                                             (cascade-nonlocal all-args (pre-desugar body)))
                                                   (map pre-desugar decorators)
                                                   (none))))))]
       [PyListComp (body generators)
                   (LexBlock empty (LexListComp (pre-desugar body) (map pre-desugar generators)))]
       [PyGeneratorExp (body generators)
                       (LexBlock empty (LexGeneratorExp (pre-desugar body) (map pre-desugar generators)))]
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
                  [(member (first asnames) (get-builtin-module-names))
                   (append
                    (list
                     (LexAssign
                      (list (PyLexId (first asnames) 'Store))
                      (LexApp (PyLexId '__import__ 'Load)
                              (list (LexStr (first names)))
                              (list) (none) (none))))
                    (desugar-pyimport/rec (rest names) (rest asnames)))]
                  [else
                   (append
                    (list
                     (LexAssign
                                        ; assign to a module object first, in case that the imported file
                                        ; needs information in current scope
                      (list (PyLexId (first asnames) 'Store))
                      (LexApp (PyLexId '$module 'Load) (list) (list) (none) (none)))
                     (LexAssign
                      (list (PyLexId (first asnames) 'Store))
                      (LexApp (PyLexId '__import__ 'Load)
                              (list (LexStr (first names))) (list) (none) (none))))
                    (desugar-pyimport/rec (rest names) (rest asnames)))]))]
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
                              (list (LexStr module))
                              (list) (none) (none))))]
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
            (remove-unneeded-pypass
               (replace-lexmodule
                (remove-unneeded-assigns
                 (process-syntax-errors
                  (bind-locals fully-transformed)))))))) ;surround every block with PyLet of locals

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
                                         (LexPass? (LexAssign-value (first es))))
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
                                        [LexClass (scope name bases body keywords stararg kwarg decorators)
                                                  (LexClass replace-scope name
                                                            (map remove-unneeded-assigns bases)
                                                            (remove-unneeded-assigns body)
                                                            (map remove-unneeded-assigns keywords)
                                                            (option-map remove-unneeded-assigns stararg)
                                                            (option-map remove-unneeded-assigns kwarg)
                                                            (map remove-unneeded-assigns decorators))]
                                        [LexFunc (name args vararg kwonlyargs kwarg defaults kw_defaults
                                                       body decorators class)
                                                 (LexFunc name args vararg kwonlyargs kwarg
                                                          (map remove-unneeded-assigns defaults)
                                                          (map remove-unneeded-assigns kw_defaults)
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
      [
       #|(define (syntax-error [err : string]) : LexExpr
         (k 
          (LexModule
           (list (LexRaise
                  (LexApp
                   (LexGlobalId 'SyntaxError 'Load)
                   (list (LexStr err))
                   (list) (none) (none)))))))
                         
         
       (define (bindings-for-nonlocal [bindings : (listof symbol)] [expr : LexExpr]) : LexExpr
         (let ((these-locals empty))
           (lexexpr-modify-tree
               expr
               (lambda (e)
                 (type-case LexExpr e
                   [PyLexNonLocal (locals) (if
                                            (empty? (list-subtract locals bindings))
                                            (PyLexNonLocal locals)
                                            (syntax-error (string-append
                                                           "no binding for nonlocal '"
                                                           (string-append
                                                            (symbol->string (first locals))
                                                            "' found")))
                                            )]
                                  [LexBlock (nls e) (LexBlock nls (bindings-for-nonlocal
                                                                   (remove-duplicates
                                                                    (flatten (list bindings these-locals nls)))
                                                                   e))]
                                  [LexLocalId (x ctx) (begin (set! these-locals (cons x these-locals)) e)]
                                  [else (default-recur)]
                                  )))))
       (define (continue/break-errors-finally expr)
         (lexexpr-modify-tree
          expr
          (lambda [e]
            (type-case LexExpr e
              [LexContinue () (syntax-error "'continue' not supported inside 'finally clause")]
              ;break is totally supported inside finally blocks
              [LexWhile (a b c) (continue/break-errors e)]
              [LexFor (a b c) (continue/break-errors e)]
              [else (default-recur)]))))

       (define (continue/break-correct expr)
         (lexexpr-modify-tree
          expr
          (lambda (e)
            (type-case LexExpr e
              [LexBlock (nls es) (LexBlock nls (continue/break-errors es))]
              [LexTryFinally (try finally)
                             (LexTryFinally
                              (continue/break-errors try)
                              (continue/break-errors-finally finally))]
            [else (default-recur)])))
         )

       (define (continue/break-errors expr)
         (lexexpr-modify-tree
          expr
          (lambda (e)
            (type-case LexExpr e
              [LexTryFinally (try finally)
                             (LexTryFinally
                              (continue/break-errors try)
                              (continue/break-errors-finally finally))]
              [LexContinue () (syntax-error "'continue' not properly in loop")]
              [LexBreak () (syntax-error "'break' outside loop")]
              [LexWhile (test body orelse) (LexWhile
                                            (continue/break-errors test)
                                            (continue/break-correct body)
                                            (continue/break-errors orelse))]
              [LexFor (target iter body) (LexFor
                                          (continue/break-errors target)
                                          (continue/break-errors iter)
                                          (continue/break-correct body))]
              [else (default-recur)]))));|#
       
         ]
       ;(k (continue/break-errors (bindings-for-nonlocal empty expr)))
      ;we're doing the smart thing and just letting python find our synatx errors.
      expr))))




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


#|(define (assert-pyblock-exists expr)
  (if (empty? (lexexpr-fold-tree expr (lambda (x) (type-case LexExpr x
                                             [LexBlock(_) true]
                                             [else false]))
                                (lambda (x) (type-case LexExpr x
                                        [LexBlock(a) (list a)]
                                        [else (default-recur)]))))
      (error 'assert-pyblock-exists "pyblock is missing")
      expr))|#

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


(define (extract-unreplaced-locals [expr : LexExpr ] ) : (listof symbol)
  (let ((overestimate (filter (lambda (x) (not (contains-char? (symbol->string x) (chr "-") )))
           (remove-duplicates (extract-locals-helper expr))))
        (globals (extract-globals expr true)))
    (begin
      ;(display "expr: ")
      ;(lexexpr-print expr)
      ;(pretty-write expr)
      ;(display "\n\n")
      ;(display overestimate)
      ;(display "\n\n")
      ;(display globals)
      ;(display "\n\n")
      (let ((result (list-subtract overestimate globals)))
        (begin
          ;(display result)
          ;(display "\n\n END END END\n\n")
          result))
      
    )))


;takes a tree to traverse (the expression)
(define (extract-locals-helper [expr : LexExpr] ) : (listof symbol)
  (letrec ((target-fun
         (lambda ([exp : LexExpr]) : (listof symbol)
           (begin
             ;(display "saw this: \n")
             ;(lexexpr-print exp)
             ;(display "\n")
             (type-case LexExpr exp
             [PyLexId (sym ctx) (list sym)]
             [LexInstanceId (_ __) empty]
             [LexTuple (values) (flatten (map target-fun values))] 
             [else empty]))))
        (spec (lambda ([exp : LexExpr]) : (listof symbol)
                (type-case LexExpr exp
                  [LexAssign (targets value)
                             (let
                                 ((target-ids (flatten (map target-fun targets))))
                               (flatten (list
                                         (extract-locals-helper value)
                                         target-ids
                                         (flatten (map extract-locals-helper targets)))))]
                  [LexAugAssign (op target value) (flatten (list (extract-locals-helper value)
                                                                (target-fun target)
                                                                (extract-locals-helper target))) ]
                  [LexFor (target iter body orelse) (flatten (list (target-fun target)
                                                                   (extract-locals-helper target)
                                                                   (extract-locals-helper iter)
                                                                   (extract-locals-helper body)
                                                                   (extract-locals-helper orelse)))]
                  [LexComprehen (target iter ifs) (flatten (list (target-fun target)
                                                                 (extract-locals-helper target)
                                                                 (extract-locals-helper iter)
                                                                 (map extract-locals-helper ifs)))]
                  [PyLexNonLocal (ids) ids]
                  [LexBlock (nls es) empty]
                  [LexExceptAs (types name body) (list name)]
                  [else (default-recur)]))))
    (lexexpr-fold-tree expr spec)))

(define (find-all-instance expr) : (listof symbol)
      (let [[post-remove
      (list-subtract
       (list-subtract
        (lexexpr-fold-tree
         expr
         (letrec
             ((inst-lam
               (lambda ([y : LexExpr]) : (listof symbol)
                       (type-case LexExpr y
                         [PyLexId (name ctx) (list name)]
                         [LexTuple (values) (flatten (map inst-lam values))]
                         [else (find-all-instance y)]))))
           (lambda (x) : (listof symbol)
           (type-case LexExpr x
             [LexAssign (lhs rhs)
                        (flatten (list (flatten (map inst-lam lhs)) (find-all-instance rhs)))]
             [LexAugAssign (op lhs rhs) (flatten (list (inst-lam lhs) (find-all-instance rhs)))]
             [LexFor (target iter body orelse) (flatten (list (inst-lam target)
                                                              (find-all-instance iter)
                                                              (find-all-instance body)
                                                              (find-all-instance orelse)))]
             [LexComprehen (target iter ifs) (flatten (list (inst-lam target)
                                                            (find-all-instance iter)
                                                            (map find-all-instance ifs)))]
             [LexBlock (_ __) empty]
             [else (default-recur)]))))
        (extract-globals expr true))
         (extract-nonlocals expr))]]
        (begin
        post-remove)))

(define (replace-all-instance [expr : LexExpr]) : LexExpr
  (begin
    ;(display "entering replace-all-instance\n")
    
  (local
   [
    (define (toplevel [expr : LexExpr ]) : LexExpr
      (lexexpr-modify-tree
       expr
       (lambda (x)
         (type-case LexExpr x
           [LexClass (scope name bases body keywords stararg kwarg decorators)
                           (LexClass
                            scope
                            name
                            (map toplevel bases)
                            (type-case LexExpr body
                              [LexBlock (nls e)
                                       (LexBlock
                                        nls
                                        (second-level e))]
                              [else (begin (display "thing in class literal is not a block")
                                           (error 'e "thing in class literal is not block"))])
                            (map toplevel keywords)
                            (option-map toplevel stararg)
                            (option-map toplevel kwarg)
                            (map toplevel decorators))]
           [else (default-recur)]))))
    (define (second-level expr )
      (let
          ((discovered-vars (find-all-instance expr)))
        (begin
          ;(display "found some instance: ")
          ;(display discovered-vars)
          ;(display "\n\n")
        (letrec ((recur
                  (lambda ([expr : LexExpr] [locs : (listof symbol)])
                    (lexexpr-modify-tree
                     expr
                     (letrec
                         ((assign-func
                           (lambda (x) : LexExpr
                                   (type-case LexExpr x
                                     [PyLexId (y ctx) (if
                                                       (member y locs)
                                                       (LexInstanceId y ctx)
                                                       x)]
                                     [LexTuple (vals) (LexTuple (map assign-func vals))]
                                     [else x]))))
                       (lambda ([x : LexExpr]) : LexExpr
                         (type-case LexExpr x
                                        ;[LexId (e) (LexInstanceId e)]
                           [LexAssign (targets value) (LexAssign (map assign-func targets)
                                                                 (recur value locs))]
                           [LexAugAssign (op target value) (LexAugAssign op (assign-func target)
                                                                         (recur value locs))]
                           [LexBlock (nls e) (LexBlock nls (toplevel e))]
                           [LexFor (target iter body orelse) (LexFor (assign-func target)
                                                                     (recur iter locs)
                                                                     (recur body locs)
                                                                     (recur orelse locs)) ]
                           [LexComprehen (target iter ifs) (LexComprehen (assign-func target)
                                                                         (recur iter locs)
                                                                         (map (lambda (e) (recur e locs)) ifs))]
                           [LexClass (scope name bases body keywords stararg kwarg decorators) (toplevel x)]
                           [else (default-recur)])))))))
          (recur expr discovered-vars)
          ))))
    ]
   (toplevel expr))))
      
(define (all-replaced-instance [es : LexExpr])
  (lexexpr-fold-tree
   es
   (lambda ( [e : LexExpr] )
     (type-case LexExpr e
       [LexInstanceId (x ctx) (list x)]
       [LexBlock (_ __) empty]
       [else (default-recur)]))))

(define (replace-all-locals [expr : LexExpr] [locs : (listof symbol) ] [instance : (listof symbol)])
  (begin
    ;(display "entering replace-all-locals\n")
    ;(lexexpr-print expr)
    ;(display "\n")
    (let ((replace (lambda ([str : symbol] [ctx : symbol])
                     (if (empty? (flatten
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
         [else (default-recur)]))))))

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
                                        ;[LexReturn (r) (LexReturn (option-map make-all-global r))]
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
                 [LexTuple (values) (flatten (map targ-fun values))]
                 [else empty]
                 )))
            (spec (lambda (exp) (type-case LexExpr exp
                 [LexAssign (targets value)
                           (let
                               ((target-ids (flatten (map targ-fun
                                                 targets))))
                             (flatten (list
                                       (extract-locals-helper value)
                                       target-ids
                                       (flatten (map extract-locals-helper targets)))))]
                 [LexAugAssign (op l r) (flatten (list (extract-locals-helper r)
                                                       (targ-fun l)
                                                       (extract-locals-helper l)))]
                 [LexFor (target iter body orelse) (flatten (list
                                                             (extract-locals-helper target)
                                                             (targ-fun target)
                                                             (extract-locals-helper iter)
                                                             (extract-locals-helper body)
                                                             (extract-locals-helper orelse)))]
                 [LexComprehen (target iter ifs) (flatten (list
                                                           (extract-locals-helper target)
                                                           (targ-fun target)
                                                           (extract-locals-helper iter)
                                                           (map extract-locals-helper ifs)))]
                 [LexBlock (nls es) empty]
                 [LexExceptAs (types name body) (list name)]
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

(define (remove-global expr)
  (lexexpr-modify-tree
   expr
   (lambda (x)
     (type-case LexExpr x
       [PyLexGlobal(y) (LexPass)]
       [else (default-recur)]))))
