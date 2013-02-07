#lang plai-typed

(require "python-lexical-syntax.rkt"
         "python-core-syntax.rkt"
         "util.rkt"
         "builtins/num.rkt"
         "python-syntax-operations.rkt"
         "builtins/str.rkt")
(require (typed-in racket/base (number->string : (number -> string)))
         (typed-in racket/list (last : ((listof 'a) -> 'a)))
         (typed-in racket/list (count : (('a -> boolean) (listof 'a) -> number)))
         (typed-in racket/base (append : ((listof 'a) (listof 'a) -> (listof 'a)))))


(define (desugar-boolop [op : symbol] [values : (listof LexExpr)]) : CExpr
  (local [(define first-val (rec-desugar (first values)))]
    (if (> (length values) 1)
        (local [(define rest-val (desugar-boolop op (rest values)))]
          (case op
            ['And (CIf first-val
                       rest-val 
                       first-val)]
            ['Or (CIf first-val
                      first-val
                      rest-val)]))
        first-val)))

(define (desugar-compop [l : LexExpr] [ops : (listof symbol)] 
                        [comparators : (listof LexExpr)]) : CExpr
    (local [(define first-comp
              (rec-desugar (LexBinOp l (first ops) (first comparators))))]
      (if (> (length comparators) 1) 
          (local [(define rest-comp
                    (desugar-compop (first comparators)
                                    (rest ops)
                                    (rest comparators)))]
                 (CIf first-comp rest-comp first-comp))
          first-comp)))

<<<<<<< HEAD
(define (desugar-pymodule [es : (listof LexExpr)]) : CExpr
  (local [(define prelude
            (rec-desugar (LexPass))
            #;(if (not (empty? names))
                (rec-desugar (LexSeq (map (lambda (n) (LexAssign (list (LexId n 'Load))
                                                               (LexUndefined)))
                                         names))
                              g/ns-)
                (rec-desugar (LexPass)  g/ns-)))
          (define body (rec-desugar (LexSeq es)))]
     (CModule prelude body)))

(define (desugar-for [target : LexExpr] [iter : LexExpr]
                     [body : LexExpr]) : CExpr
  (local [(define iter-pyid (LexLocalId (new-id) 'Load))]
=======
;; look through a  and find a list of all names from assignments and definition
;; if global scope, it only gets definitions, for local scope it gets
;; definitions and assignments
(define (get-names [expr : PyExpr] [global? : boolean] [env : IdEnv]) : (listof symbol)
  (type-case PyExpr expr
    [PyIf (t b e) (append (get-names b global? env)
                          (get-names e global? env))]
    [PySeq (es) (foldl (lambda(e so-far) (append (get-names e global? env) so-far))
                       empty
                       es)]
    ; ignore special variables that we've defined in desugaring for which 
    ; we have specially constructed the scope manually for purposes of 
    ; desugaring
    [PyId (id ctx) (if (not (symbol=? ctx 'DesugarVar)) 
                       (list id)
                       empty)]
    [PyAssign (targets v) (begin
                            ;(display targets) (display "\n")  (display v) (display "\n\n")
                            (if (and (not global?) (not (PySubscript? (first targets))))
                                (foldl (lambda(t so-far) (append (get-names t global? env)
                                                                 so-far))
                                       empty
                                       targets)
                                empty))]
    [PyExcept (t body) (get-names body global? env)]
    [PyTryExceptElseFinally (t e o f)
                            (append (get-names t global? env)
                                    (append (foldl (lambda(e so-far) (append
                                                                      (get-names e
                                                                                 global?
                                                                                 env)
                                                                      so-far))
                                                   empty 
                                                   e)
                                            (append (get-names o global? env)
                                                    (get-names f global? env))))]
    [PyClass (name bases body) (list name)]
    [PyBinOp (l o r) (append (get-names l global? env)
                             (get-names r global? env))]
    [PyUnaryOp (o operand) (get-names operand global? env)]
    [PyFunc (name args defvargs body decorators) (list name)]
    [PyFuncVarArg (name args sarg body decorators) (list name)]
    [else empty]))

(define (get-right-names [expr : PyExpr] [global? : boolean] [env : IdEnv]) : (listof symbol)
  (type-case PyExpr expr
    [PyIf (t b e) (append (get-right-names b global? env)
                          (get-right-names e global? env))]
    [PySeq (es) (foldl (lambda(e so-far) (append (get-right-names e global? env) so-far))
                       empty
                       es)]
    ; ignore special variables that we've defined in desugaring for which 
    ; we have specially constructed the scope manually for purposes of 
    ; desugaring
    [PyId (id ctx) (if (not (symbol=? ctx 'DesugarVar)) 
                       (list id)
                       empty)]
    [PyAssign (targets v) (begin
                            ;(display targets) (display "\n")  (display v) (display "\n\n")
                            (if (not global?)
                                (get-right-names v global? env)
                                empty))]
    [PyExcept (t body) (get-right-names body global? env)]
    [PyTryExceptElseFinally (t e o f)
                            (append (get-right-names t global? env)
                                    (append (foldl (lambda(e so-far) (append
                                                                      (get-right-names e
                                                                                       global?
                                                                                       env)
                                                                      so-far))
                                                   empty 
                                                   e)
                                            (append (get-right-names o global? env)
                                                    (get-right-names f global? env))))]
    [PyBinOp (l o r) (append (get-right-names l global? env)
                             (get-right-names r global? env))]
    [PyUnaryOp (o operand) (get-right-names operand global? env)]
    [else empty]))

(define (get-globals/nonlocals [expr : PyExpr] [global? : boolean]
                               [env : IdEnv]) : IdEnv
  (local [(define (rec-get-g/ns [exprs : (listof PyExpr)]
                                [global? : boolean]
                                [env : IdEnv]) : IdEnv
            (cond
              [(empty? exprs) env]
              [else (rec-get-g/ns (rest exprs) global?
                                  (get-globals/nonlocals (first exprs) global? env))]))]
    (type-case PyExpr expr
      [PyIf (t b e) (get-globals/nonlocals e global? env)]
      [PySeq (es) (rec-get-g/ns es global? env)]
      [PyId (id ctx) (begin ;(display id) (display "\n")
                       ;(display env) (display "\n")
                       (local [(define type (lookup-idtype id env))]
                         (begin
                           ;(display "type: ") (display type) (display "\n\n")
                           (if (some? type)
                               env 
                               (add-id id (if global?
                                              (GlobalId)
                                              (LocalId))
                                       env)))))]
      [PyGlobal (ids) (begin ;(display "global: ") (display ids) (display "\n")
                        (add-ids ids (GlobalId) env))]
      [PyNonlocal (ids) (begin ;(display "nonlocal: ") (display ids) (display "\n")
                          ;(display env) (display "\n\n")
                          (add-ids ids (NonlocalId) env))]
      [PyAssign (targets v) (rec-get-g/ns targets global? env)]
      [PyAugAssign (o t v) (get-globals/nonlocals v global? 
                                                  (get-globals/nonlocals t global? env))]
      [PyExcept (t body) (get-globals/nonlocals body global? env)]
      [PyTryExceptElseFinally (t e o f)
                              (get-globals/nonlocals f global?
                                                     (get-globals/nonlocals o global? 
                                                                            (rec-get-g/ns e global? 
                                                                                          (get-globals/nonlocals t global? env))))]
      [PyClass (name bases body)
               (local [(define type (lookup-idtype name env))]
                 (if (some? type)
                     env
                     (add-id name (LocalId) env)))]
      [PyBinOp (l o r) (get-globals/nonlocals r global?
                                              (get-globals/nonlocals l global? env))]
      [PyUnaryOp (o operand) (get-globals/nonlocals operand global? env)]
      [PyFunc (name args defargs body decorators) 
              (local [(define type (lookup-idtype name env))]
                (if (some? type)
                    env 
                    (add-id name (if global?
                                     (GlobalId)
                                     (LocalId))
                            env)))]
      [PyFuncVarArg (name args sarg body decorators)
                    (local [(define type (lookup-idtype name env))]
                      (if (some? type)
                          env
                          (add-id name (if global?
                                           (GlobalId)
                                           (LocalId))
                                  env)))]
      [else env])))


(define (desugar-pymodule [es : (listof PyExpr)] 
                          [global? : boolean]
                          [env : IdEnv]) : DesugarResult
  (local [(define g/ns-env (get-globals/nonlocals (PySeq es) global? env))
          (define names  (get-names (PySeq es) global? g/ns-env))
          (define prelude 
            (if (not (empty? names))
                (rec-desugar (PySeq (map (lambda (n) (PyAssign (list (PyId n 'Load))
                                                               (PyUndefined)))
                                         names))
                             global? g/ns-env (none))
                (rec-desugar (PyPass) global? g/ns-env (none))))
          (define body (rec-desugar (PySeq es) global? (DResult-env prelude) (none)))]
    (DResult
     (CModule
      (DResult-expr prelude)
      (DResult-expr body))
     (DResult-env body))))

(define (map-desugar [exprs : (listof PyExpr)]
                     [global? : boolean]
                     [env : IdEnv] [opt-class : (optionof symbol)]): ((listof CExpr) * IdEnv)
  (local [(define (rec-map-desugar exps g e)
            (cond
              [(empty? exps) (values empty e)]
              [(cons? exps)
               (local [(define first-r (rec-desugar (first exps) g e opt-class))
                       (define-values (results last-env)
                         (rec-map-desugar (rest exps) g (DResult-env first-r)))]
                 (values
                  (cons (DResult-expr first-r)
                        results)
                  last-env))]))]
    (rec-map-desugar exprs global? env)))

(define (assign-undef [s : symbol]) : PyExpr
  #|
  this seems like a good idea but it breaks a test so i don't know
  (PyTryExceptElseFinally 
    (PyId s 'Load)
    (list 
      (PyExcept (list (PyId 'NameError 'Load))
                (PyAssign (list (PyId s 'Load))
                          (PyUndefined))))
    (PyPass)
    (PyPass)))|#
  (PyAssign (list (PyId s 'Load)) (PyUndefined)))

(define (assign-val [s : symbol]) : PyExpr
  (PyAssign (list (PyId s 'Load)) (PyId s 'Load)))

;; for the body of some local scope level like a class or function, hoist
;; all the assignments and defs to the top as undefineds
(define (desugar-local-body [expr : PyExpr] [args : (listof symbol)]
                            [env : IdEnv]) : DesugarResult
  (local [(define g/ns-env (get-globals/nonlocals expr false empty))
          (define names (get-names expr false g/ns-env))
          (define right-names (filter (λ (n) (not (member n names)))
                                      (get-right-names expr false g/ns-env)))]
    (begin 
      (rec-desugar
       (if (not (empty? names))
           (PySeq (append 
                   (map assign-undef
                        (filter (lambda(n) (not (member n args)))
                                names))
                   (append (map assign-val right-names)
                           (list expr))))
           expr)
       false
       g/ns-env
       (none)))))

(define (desugar-for [target : PyExpr] [iter : PyExpr] [body : PyExpr]
                     [global? : boolean] [env : IdEnv]) : DesugarResult
  (local [(define iter-pyid (PyId (new-id) 'Load))]
>>>>>>> master
    (rec-desugar
     (LexSeq
      (list (LexAssign (list iter-pyid) (LexApp (LexGlobalId 'iter 'Load) (list iter)))
            (LexWhile (LexBool true)
                     (LexSeq 
                      (list 
                       (LexAssign (list target) (LexNone))
                       (LexTryExceptElseFinally
                         (LexAssign (list target) 
                                    (LexApp (LexDotField iter-pyid '__next__) empty))
                         (list (LexExcept (list (LexGlobalId 'StopIteration 'Load))
                                          (LexBreak)))
                         (LexPass)
                         (LexPass))
                       body))
<<<<<<< HEAD
                     (LexPass)))))))
=======
                     (PyPass))))
     global?
     env
     (none))))
>>>>>>> master

(define (desugar-listcomp [body : LexExpr] [gens : (listof LexExpr)] ) : CExpr
  (local [(define list-id (LexLocalId (new-id) 'Load))
          (define (make-comploop gens)
            (cond 
              [(empty? gens) (LexApp (LexDotField list-id 'append) 
                                    (list body))]
              [(cons? gens)
               (LexFor (LexComprehen-target (first gens))
                      (LexComprehen-iter (first gens))
                      (make-comploop (rest gens)))]))
          (define full-expr
            (LexSeq
             (list 
              (LexAssign (list list-id) (LexList empty))
              (make-comploop gens)
              list-id)))]
<<<<<<< HEAD
    (rec-desugar full-expr)))

(define (which-scope [scp : LocalOrGlobal]) : IdType
  (type-case LocalOrGlobal scp
    [Locally-scoped () (LocalId)]
    [Globally-scoped () (GlobalId)]
    [Unknown-scope () (error 'desugar:
                             "somehow an Unknown-scope got past the lexical stage.")]
    [Instance-scoped () (error 'desugar:
                               "this is an instance variable, please handle it accordingly")]))

(define (desugar-seq [es : (listof LexExpr)]) : CExpr
  (cond
   [(empty? es) (error 'desugar "empty sequences are not supported")]
   [(empty? (rest es)) (desugar (first es))]
   [else (CSeq (desugar (first es)) (desugar-seq (rest es)))]))

#|  
=======
    (rec-desugar full-expr global? env (none))))
>>>>>>> master

;;; desugar import name as asname to
;;; asname = __import__('name')
(define (desugar-import-py names asnames) : PyExpr
  (local [(define (helper names asnames result)
            (cond [(empty? names) result]
                  [else
                   (helper (rest names) (rest asnames)
                           (append result
                                   (list (PyAssign (list (PyId (first asnames) 'Store))
                                                   (PyApp (PyId '__import__ 'Load)
                                                          (list (PyStr (first names))))))))]))]
         (PySeq (helper names asnames (list)))))

;;; desugar from module import name as asname
;;; __tmp_module = __import__(module, globals(), locals(), [id], 0)
;;; id_alias = __tmp_module.id
;;; 
;;;  from module import * will desugar to
;;; __import__all__(__import__(module, globals(), locals(), ['*'], 0)) 
;;; where __import__all__ performs importing based on __all__
(define (desugar-importfrom-py [module : string]
                               [names : (listof string)]
                               [asnames : (listof symbol)]
                               [level : number]) : PyExpr
  (local [(define tmp-module (PyId '__tmp_module 'Store))
          (define module-expr
            (PyAssign (list tmp-module)
                      (PyApp (PyId '__import__ 'Load)
                             (list (PyStr module)
                                   (PyApp (PyId '__globals 'Load) (list))
                                   (PyApp (PyId '__locals 'Load) (list))
                                   (PyList (map PyStr names)) ; list type of names
                                   (PyNum level)))))]
    (cond [(not (equal? (first names) "*"))
           (local [(define (get-bind-exprs module names asnames)
                     (cond [(empty? names) (list)]
                           [else
                            (append (list (PyAssign (list (PyId (first asnames) 'Store))
                                                    (PyDotField module (string->symbol (first names)))))
                                    (get-bind-exprs module (rest names) (rest asnames)))]))
                   (define bind-exprs
                     (get-bind-exprs tmp-module names asnames))]
             (PySeq (append (list module-expr) bind-exprs)))]
          [else ;; from module import *
           (PyApp (PyId '__import__all__ 'Load)
                  (list (PyApp (PyId '__import__ 'Load)
                               (list (PyStr module)
                                     (PyApp (PyId '__globals 'Load) (list))
                                     (PyApp (PyId '__locals 'Load) (list))
                                     (PyList (list (PyStr "*"))) ; list type of names
                                     (PyNum level)))))])))

<<<<<<< HEAD

|#

(define (rec-desugar [expr : LexExpr] ) : CExpr 
  (begin ;(display expr) (display "\n\n")
    (type-case LexExpr expr
      [LexSeq (es) (desugar-seq es)]
      [LexModule (es) (desugar-pymodule es)]
      [LexAssign (targets value) 
                (type-case LexExpr (first targets) 
                  ; We handle three kinds of assignments.
                  ; An assignment to a subscript is desugared as a __setitem__ call.
                  [LexSubscript (left ctx slice)
                               (letrec ([desugared-target (rec-desugar left)]
                                        [desugared-slice 
                                         (rec-desugar slice)]
                                        [desugared-value
                                         (rec-desugar value)]
                                        [target-id (new-id)])
                                 (CApp (CGetField desugared-target '__setitem__)
                                        (list desugared-target
                                              desugared-slice
                                              desugared-value)
                                        (none)))]
                  ; An assignment to a tuple is desugared as multiple __setitem__ calls.
                  [LexTuple (vals)
                           (local [(define targets-r (map rec-desugar vals))
                                   (define value-r (rec-desugar value))
=======
(define (rec-desugar [expr : PyExpr] [global? : boolean]
                     [env : IdEnv] [opt-class : (optionof symbol)]) : DesugarResult 
  (begin ;(display expr) (display "\n\n")
    (type-case PyExpr expr
      [PySeq (es) (local [(define-values (exprs-r last-env)
                            (map-desugar es global? env opt-class))]
                    (DResult
                     (foldl (lambda (e1 so-far) (CSeq so-far e1))
                            (first exprs-r)
                            (rest exprs-r))
                     last-env))]
      [PyModule (es) (desugar-pymodule es global? env)]
      [PyAssign (targets value) 
                (type-case PyExpr (first targets) 
                  ; We handle three kinds of assignments.
                  ; An assignment to a subscript is desugared as a __setitem__ call.
                  [PySubscript (left ctx slice)
                               (letrec ([desugared-target (rec-desugar left global? env (none))]
                                        [desugared-slice 
                                         (rec-desugar slice global? (DResult-env desugared-target) (none))]
                                        [desugared-value
                                         (rec-desugar value global? (DResult-env desugared-slice) (none))]
                                        [target-id (new-id)])
                                 (DResult
                                  (CApp (CGetField (DResult-expr desugared-target) '__setitem__)
                                        (list (DResult-expr desugared-slice)
                                              (DResult-expr desugared-value))
                                        (none))
                                  (DResult-env desugared-value)))]
                  ; An assignment to a tuple is desugared as multiple __setitem__ calls.
                  [PyTuple (vals)
                           (local [(define-values (targets-r mid-env) (map-desugar vals global?
                                                                                   env (none)))
                                   (define value-r (rec-desugar value global? mid-env (none)))
>>>>>>> master
                                   (define assigns
                                     (map2 (λ (t n) 
                                             (CAssign t (CApp
                                                         (CGetField (CId '$tuple_result (LocalId)) 
                                                                    '__getitem__)
                                                         (list (make-builtin-num n))
                                                         (none))))
                                           targets-r
                                           (build-list (length targets-r) identity)))]
                              (CLet '$tuple_result (LocalId) value-r 
                                    (foldl (λ (a so-far) (CSeq so-far a))
                                           (first assigns) (rest assigns))))]
                  ; The others become a CAssign.
                  [else
<<<<<<< HEAD
                   (local [(define targets-r (map rec-desugar targets))
                           (define value-r (rec-desugar value))]
                          (foldl (lambda (t so-far)
                                   (CSeq so-far (CAssign t value-r)))
                                 (CAssign (first targets-r) value-r)
                                 (rest targets-r)))])]
=======
                   (local [(define-values (targets-r mid-env)
                             (map-desugar targets global? env (none)))
                           (define value-r (rec-desugar value global? mid-env (none)))]
                     (DResult
                      (foldl (lambda (t so-far)
                               (CSeq so-far (CAssign t (DResult-expr value-r))))
                             (CAssign (first targets-r) (DResult-expr value-r))
                             (rest targets-r))
                      (DResult-env value-r)))])]
>>>>>>> master
      
      [LexNum (n) (make-builtin-num n)]
      [LexSlice (lower upper step) (error 'desugar "Shouldn't desugar slice directly")]
      [LexBool (b) (if b (CTrue) (CFalse))]
      [LexNone () (CNone)]
      [LexStr (s) (make-builtin-str s)]
      [LexLocalId (x ctx) (CId x (LocalId))]
      [LexGlobalId (x ctx) (CId x (GlobalId))]
      [LexGlobalLet (x bind body) (CLet x (GlobalId) (rec-desugar bind) (rec-desugar body))]
      [LexLocalLet (x bind body) (CLet x (LocalId) (rec-desugar bind) (rec-desugar body))]
      [LexUndefined () (CUndefined)]  
      
<<<<<<< HEAD
      [LexRaise (expr) (local [(define expr-r
                                 (if (or (LexLocalId? expr) (LexGlobalId? expr))
                                     ;;handle the implicit construction case
                                     (rec-desugar (LexApp expr empty)) 
                                     (rec-desugar expr)))]
=======
      [PyRaise (expr) (local [(define expr-r (if (PyId? expr)
                                                 ;;handle the implicit construction case
                                                 (rec-desugar (PyApp expr empty)
                                                              global? env (none)) 
                                                 (rec-desugar expr global? env (none))))]
                        (DResult
>>>>>>> master
                         (CRaise 
                          (if (LexPass? expr)
                              (none)
<<<<<<< HEAD
                              (some expr-r))))]
      
      ; LexPass is an empty lambda
      [LexPass () (CApp (CFunc empty (none) (CNone) false) empty (none))] 
      
      [LexIf (test body orelse)
            (local [(define test-r (rec-desugar test))
                    (define body-r (rec-desugar body))
                    (define orelse-r (rec-desugar orelse))]
               (CIf test-r
                    body-r
                    orelse-r))]
      
      [LexBinOp (left op right)
               (local [(define left-r (rec-desugar left))
                       (define left-c left-r)
                       (define right-r (rec-desugar right))
                       (define right-c right-r)] 
                 (case op 
                   ['Add (CApp (CGetField left-c '__add__) 
                               (list left-c right-c)
                               (none))]
                   ['Sub (CApp (CGetField left-c '__sub__) 
                               (list left-c right-c)
                               (none))]
                   ['Mult (CApp (CGetField left-c '__mult__)
                                (list left-c right-c)
                                (none))]
                   ['Div (CApp (CGetField left-c '__div__)
                               (list left-c right-c)
                               (none))]
                   ['FloorDiv (CApp (CGetField left-c '__floordiv__)
                                    (list left-c right-c)
                                    (none))]
                   ['Mod (CApp (CGetField left-c '__mod__)
                               (list left-c right-c)
                               (none))]
                   ['BitAnd (CApp (CGetField left-c '__and__)
                                  (list left-c right-c)
                                  (none))]
                   ['BitOr (CApp (CGetField left-c '__or__)
                                 (list left-c right-c)
                                 (none))]
                   ['BitXor (CApp (CGetField left-c '__xor__)
                                  (list left-c right-c)
                                  (none))]
                   ['Eq (CApp (CGetField left-c '__eq__)
                              (list left-c right-c)
                              (none))]
                   ['Gt (CApp (CGetField left-c '__gt__)
                              (list left-c right-c)
                              (none))]
                   ['Lt (CApp (CGetField left-c '__lt__)
                              (list left-c right-c)
                              (none))]
                   ['LtE (CApp (CGetField left-c '__lte__)
                               (list left-c right-c)
                               (none))]
                   ['GtE (CApp (CGetField left-c '__gte__)
                               (list left-c right-c)
                               (none))]
                   ['NotEq (rec-desugar (LexUnaryOp 'Not (LexBinOp left 'Eq right)))]
                   ['In (CApp (CFunc (list 'self 'test) (none)
                                     (CLet '__infunc__ (LocalId)
                                           (CGetField (CId 'self (LocalId))
                                                      '__in__)
                                           (CIf (CId '__infunc__ (LocalId))
                                                (CReturn
                                                  (CApp
                                                    (CId '__infunc__ (LocalId))
                                                    (list (CId 'self (LocalId))
                                                          (CId 'test (LocalId)))
                                                    (none)))
                                                (CRaise (some
                                                  (CApp (CId 'TypeError (LocalId))
                                                        (list (CObject
                                                                'str
                                                                (some (MetaStr 
                                                                        (string-append
                                                                          "argument of type '___'" 
                                                                          "is not iterable")))))
                                                        (none))))))
                                     false)
                              (list right-c left-c)
                              (none))]
                   ['NotIn (rec-desugar (LexUnaryOp 'Not (LexBinOp left 'In right)))]
                   [else (CPrim2 op left-c right-c)]))]

      [LexUnaryOp (op operand)
                  (case op
                    ['USub (rec-desugar (LexBinOp (LexNum 0) 'Sub operand))]
                    ['UAdd (rec-desugar (LexBinOp (LexNum 0) 'Add operand))]
                    ['Invert (local [(define roperand (rec-desugar operand))]
                               (CApp (CGetField roperand '__invrt__)
                                     (list roperand)
                                     (none)))]
                    [else (CPrim1 op (rec-desugar operand))])]
      
      [LexBoolOp (op values) (desugar-boolop op values)]
      [LexCompOp (l op rights) (desugar-compop l op rights)]
      [LexListComp (elt gens) (desugar-listcomp elt gens)]
      [LexComprehen (target iter) (error 'desugar "Can't desugar LexComprehen")]
      
      [LexLam (args body) (CFunc args (none) (CReturn (rec-desugar body)) false)]

      [LexFunc (name args defargs body)
               (if (> (length defargs) 0)
                   (local [(define last-arg (first (reverse args)))]
                     (rec-desugar
                       ; assuming 1 defarg for now, generalize later
                       (LexSeq 
                         (list
                           (LexAssign (list (LexLocalId last-arg 'DesugarVar))
                                      (first (reverse defargs)))
                           (LexFuncVarArg name empty
                                          'stararg 
                                          (LexSeq
                                            (list
                                              (LexIf (LexCompOp (LexApp
                                                                  (LexDotField
                                                                    (LexLocalId 'stararg 'Load)
                                                                    '__len__)
                                                                  empty)
                                                                (list 'Gt)
                                                                (list (LexNum 0)))
                                                     (LexAssign (list (LexLocalId last-arg
                                                                                  'DesugarVar))
                                                                (LexSubscript
                                                                  (LexLocalId 'stararg 'Load)
                                                                  'Load
                                                                  (LexNum 0)))
                                                     (LexPass))
                                              body)))))))
                   (local [(define body-r (rec-desugar body))]
                     (CFunc args (none) body-r false)))]
      
      ; a LexClassFunc is a method whose first argument should be the class rather than self
      [LexClassFunc (name args body)
                    (local [(define body-r (rec-desugar body))]
                           (CFunc args (none)
                                        ; We do this by, inside the function body,
                                        ; taking the first argument, which is "self",
                                        ; using that to look up the object's class, and then
                                        ; "overwriting" the first argument with that value.
                                        ; The result is that, in the function body, the first
                                        ; argument is the class, as expected.
                                  (CSeq (CAssign (CId (first args) (LocalId))
                                                 (CBuiltinPrim '$class
                                                               (list (CId (first args)
                                                                          (LocalId)))))
                                        body-r)
                                  false))]
      
      [LexFuncVarArg (name args sarg body)
                     (CFunc args (some sarg) (rec-desugar body) false)]
      
      [LexReturn (value) (CReturn (rec-desugar value))]
      
      [LexDict (keys values) (CDict (lists->hash (map rec-desugar keys)
                                                 (map rec-desugar values)))]
      [LexSet (elts) (CSet (map rec-desugar elts))]
      [LexList (values) (CList (map rec-desugar values))]
      [LexTuple (values) (CTuple (map rec-desugar values))]
      
      [LexSubscript (left ctx slice)
                    (cond
                      [(symbol=? ctx 'Load)
                       (local [(define left-id (new-id))
                               (define left-var (CId left-id (LocalId)))
                               (define left-r (rec-desugar left))]
                         (if (LexSlice? slice)
                             (local [(define slice-low (rec-desugar (LexSlice-lower slice)))
                                     (define slice-up (rec-desugar (LexSlice-upper slice)))
                                     (define slice-step (rec-desugar (LexSlice-step slice)))]
=======
                              (some (DResult-expr expr-r))))
                         (DResult-env expr-r)))]

      ;; assert check is always enabled, it doesn't test __debug__ builtin variable.
      [PyAssert (test msg)
                (rec-desugar
                 (PyIf test
                       (PyPass)
                       (PyRaise (PyApp (PyId 'AssertionError 'Load) msg)))
                 global? env (none))]

      ; PyPass is an empty lambda
      [PyPass () (DResult (CApp (CFunc empty (none) (CNone) (none)) empty (none)) env)] 
      
      [PyIf (test body orelse)
            (local [(define test-r (rec-desugar test global? env (none)))
                    (define body-r (rec-desugar body global? (DResult-env test-r) (none)))
                    (define orelse-r (rec-desugar orelse global? (DResult-env body-r) (none)))]
              (DResult 
               (CIf (DResult-expr test-r)
                    (DResult-expr body-r)
                    (DResult-expr orelse-r))
               (DResult-env orelse-r)))]
      
      [PyBinOp (left op right)
               (local [(define left-r (rec-desugar left global? env (none)))
                       (define left-c (DResult-expr left-r))
                       (define right-r (rec-desugar right global? (DResult-env left-r) (none)))
                       (define right-c (DResult-expr right-r))] 
                 (case op 
                   ['Add (DResult (CApp (CGetField left-c '__add__) 
                                        (list right-c)
                                        (none))
                                  (DResult-env right-r))]
                   ['Sub (DResult (CApp (CGetField left-c '__sub__) 
                                        (list right-c)
                                        (none))
                                  (DResult-env right-r))]
                   ['Mult (DResult (CApp (CGetField left-c '__mult__)
                                         (list right-c)
                                         (none))
                                   (DResult-env right-r))]
                   ['Div (DResult (CApp (CGetField left-c '__div__)
                                        (list right-c)
                                        (none))
                                  (DResult-env right-r))]
                   ['FloorDiv (DResult (CApp (CGetField left-c '__floordiv__)
                                             (list right-c)
                                             (none))
                                       (DResult-env right-r))]
                   ['Mod (DResult (CApp (CGetField left-c '__mod__)
                                        (list right-c)
                                        (none))
                                  (DResult-env right-r))]
                   ['BitAnd (DResult (CApp (CGetField left-c '__and__)
                                           (list right-c)
                                           (none))
                                     (DResult-env right-r))]
                   ['BitOr (DResult (CApp (CGetField left-c '__or__)
                                          (list right-c)
                                          (none))
                                    (DResult-env right-r))]
                   ['BitXor (DResult (CApp (CGetField left-c '__xor__)
                                           (list right-c)
                                           (none))
                                     (DResult-env right-r))]
                   ['Eq (DResult (CApp (CGetField left-c '__eq__)
                                       (list right-c)
                                       (none))
                                 (DResult-env right-r))]
                   ['Gt (DResult (CApp (CGetField left-c '__gt__)
                                       (list right-c)
                                       (none))
                                 (DResult-env right-r))]
                   ['Lt (DResult (CApp (CGetField left-c '__lt__)
                                       (list right-c)
                                       (none))
                                 (DResult-env right-r))]
                   ['LtE (DResult (CApp (CGetField left-c '__lte__)
                                        (list right-c)
                                        (none))
                                  (DResult-env right-r))]
                   ['GtE (DResult (CApp (CGetField left-c '__gte__)
                                        (list right-c)
                                        (none))
                                  (DResult-env right-r))]
                   ['NotEq (rec-desugar (PyUnaryOp 'Not (PyBinOp left 'Eq right)) 
                                        global? env (none))]
                   ['In (DResult 
                         (CApp (CFunc (list 'self 'test) (none)
                                      (CSeq
                                       (CAssign (CId '__infunc__ (LocalId))
                                                (CGetField (CId 'self (LocalId))
                                                           '__in__))
                                       (CIf (CId '__infunc__ (LocalId))
                                            (CReturn
                                             (CApp
                                              (CId '__infunc__ (LocalId))
                                              (list (CId 'test (LocalId)))
                                              (none)))
                                            (CApp (CId 'TypeError (LocalId))
                                                  (list (CObject
                                                         'str
                                                         (some (MetaStr 
                                                                (string-append
                                                                 "argument of type '___'" 
                                                                 "is not iterable")))))
                                                  (none))))
                                      (none))
                               (list right-c left-c)
                               (none))
                         (DResult-env right-r))]
                   ['NotIn (rec-desugar (PyUnaryOp 'Not (PyBinOp left 'In right))
                                        global? env (none))]
                   [else (DResult
                          (CPrim2 op left-c right-c)
                          (DResult-env right-r))]))]
      
      [PyUnaryOp (op operand)
                 (case op
                   ['USub (rec-desugar (PyBinOp (PyNum 0) 'Sub operand) global? env (none))]
                   ['UAdd (rec-desugar (PyBinOp (PyNum 0) 'Add operand) global? env (none))]
                   ['Invert (local [(define roperand (rec-desugar operand global? env (none)))]
                              (DResult 
                               (CApp (CGetField (DResult-expr roperand) '__invrt__)
                                     (list)
                                     (none))
                               (DResult-env roperand)))]
                   [else (local [(define roperand (rec-desugar operand global? env (none)))]
                           (DResult (CPrim1 op 
                                            (DResult-expr roperand)) 
                                    (DResult-env roperand)))])]
      
      [PyBoolOp (op values) (desugar-boolop op values global? env)]
      
      [PyCompOp (l op rights) (local [(define c (desugar-compop l op rights global? env))]
                                (begin ;(display c) (display "\n")
                                  c))]
      
      [PyListComp (elt gens) (desugar-listcomp elt gens global? env)]
      [PyComprehen (target iter) (error 'desugar "Can't desugar PyComprehen")]
      
      
      [PyLam (args body)
             (local [(define rbody (rec-desugar body global? env (none)))]
               (DResult 
                (CFunc args (none)
                       (CReturn                   
                        (DResult-expr rbody))
                       (none))
                (DResult-env rbody)))]
      
      [PyFunc (name args defargs body decorators)
              (cond
                ;; function with default arguments -> converted to vararg
                [(> (length defargs) 0)
                 (local [(define last-arg (first (reverse args)))]
                   (rec-desugar
                    ; assuming 1 defarg for now, generalize later
                    (PySeq 
                     (list
                      (PyAssign (list (PyId last-arg 'DesugarVar))
                                (first (reverse defargs)))
                      (PyFuncVarArg name empty
                                    'stararg 
                                    (PySeq
                                     (list
                                      (PyIf (PyCompOp (PyApp
                                                       (PyDotField (PyId 'stararg 'Load)
                                                                   '__len__)
                                                       empty)
                                                      (list 'Gt)
                                                      (list (PyNum 0)))
                                            (PyAssign (list (PyId last-arg
                                                                  'DesugarVar))
                                                      (PySubscript (PyId 'stararg 'Load)
                                                                   'Load
                                                                   (PyNum 0)))
                                            (PyPass))
                                      body))
                                    decorators))) 
                    global? 
                    env
                    (none)))]
                ;; no default arguments nor decorators
                [(empty? decorators)
                 (local [(define body-r (desugar-local-body body args env))]
                   (DResult
                    (CAssign (CId name (LocalId))
                             (CFunc args (none) (DResult-expr body-r) opt-class))
                    env))]
                ;; no default arguments, take care of decorators
                [else
                 (rec-desugar (PySeq
                               (list
                                (PyFunc name args (list) body (list))
                                ;; apply decorators to the function
                                (PyAssign (list (PyId name 'Load))
                                          (foldr (lambda (decorator func) 
                                                   (PyApp decorator (list func)))
                                                 (PyId name 'Load)
                                                 decorators))))
                              global? env opt-class)])]
      
      [PyFuncVarArg (name args sarg body decorators)
                    (cond
                      [(empty? decorators)
                       (local [(define body-r 
                                 (desugar-local-body body (append args (list sarg)) env))]
                         (DResult
                          (CAssign (CId name (LocalId))
                                   (CFunc args (some sarg) (DResult-expr body-r) opt-class))
                          env))]
                      [else
                       (rec-desugar (PySeq
                                     (list
                                      (PyFuncVarArg name args sarg body (list))
                                      ;; apply decorators to the function
                                      (PyAssign (list (PyId name 'Load))
                                                (foldr (lambda (decorator func) 
                                                         (PyApp decorator (list func)))
                                                       (PyId name 'Load)
                                                       decorators))))
                                    global? env opt-class)])]
      
      [PyReturn (value)
                (local [(define value-r (rec-desugar value global? env (none)))]
                  (DResult (CReturn (DResult-expr value-r)) (DResult-env value-r)))]
      
      [PyDict (keys values) 
              (local [(define-values (keys-r mid-env) (map-desugar keys global? env (none)))
                      (define-values (values-r last-env)
                        (map-desugar values global? mid-env (none)))]
                (DResult (CDict (lists->hash keys-r values-r)) last-env))]
      
      [PySet (elts) (local [(define-values (results last-env)
                              (map-desugar elts global? env (none)))]
                      (DResult (CSet results) last-env))]
      
      [PyList (values) (local [(define-values (results last-env)
                                 (map-desugar values global? env (none)))]
                         (DResult (CList results) last-env))]
      
      [PyTuple (values) (local [(define-values (results last-env)
                                  (map-desugar values global? env (none)))]
                          (DResult (CTuple results) last-env))]
      
      [PySubscript (left ctx slice)
                   (cond
                     [(symbol=? ctx 'Load)
                      (local [(define left-id (new-id))
                              (define left-r (rec-desugar left global? env (none)))]
                        (if (PySlice? slice)
                            (local [(define slice-low (rec-desugar (PySlice-lower slice)
                                                                   global? env (none)))
                                    (define slice-up (rec-desugar (PySlice-upper slice)
                                                                  global? env (none)))
                                    (define slice-step (rec-desugar (PySlice-step slice)
                                                                    global? env (none)))]
                              (DResult
>>>>>>> master
                               (CLet left-id
                                     (LocalId)
                                     left-r
                                     (CApp (CGetField left-var
                                                      '__slice__)
<<<<<<< HEAD
                                           (list left-var slice-low
                                                 slice-up slice-step)
                                           (none))))
                             (local [(define slice-r (rec-desugar slice))] 
                               (CLet left-id
                                     (LocalId)
                                     left-r
                                     (CApp (CGetField left-var
                                                      '__getitem__)
                                           (list left-var slice-r)
                                           (none))))))]
                      [(symbol=? ctx 'Store)
                       (error 'desugar "bad syntax: LexSubscript has context 'Store' outside a LexAssign")]
                      [else (error 'desugar "unrecognized context in LexSubscript")])]
=======
                                           (list 
                                            (DResult-expr slice-low)
                                            (DResult-expr slice-up)
                                            (DResult-expr slice-step))
                                           (none)))
                               (DResult-env slice-step)))
                            (local [(define slice-r (rec-desugar slice global?
                                                                 (DResult-env left-r) (none)))] 
                              (DResult 
                               (CLet left-id 
                                     (DResult-expr left-r)
                                     (CSeq
                                      (CTryExceptElseFinally
                                       (CGetField (CId left-id (if global? (GlobalId) (LocalId)))
                                                      '__getitem__)
                                       (list (CExcept (list) (none) 
                                                      (CRaise (some (make-exception 
                                                                     'TypeError
                                                                     "object is not subscriptable")))))
                                       (CNone) (CNone))
                                      (CApp (CGetField (CId left-id (if global? (GlobalId) (LocalId)))
                                                       '__getitem__)
                                            (list (DResult-expr slice-r))
                                            (none))))
                               (DResult-env slice-r)))))]
                     [(symbol=? ctx 'Store)
                      (error 'desugar "bad syntax: PySubscript has context 'Store' outside a PyAssign")]
                     [else (error 'desugar "unrecognized context in PySubscript")])]
>>>>>>> master
      
      [LexBreak () (CBreak)]

      [LexContinue () (CContinue)]
      
<<<<<<< HEAD
      ;; very hacky solution for assertRaises: it needs laziness built into it, so instead
      ;; of defining it as a function, we'll special case it as a macro.
      [LexApp (fun args)
              (cond
               [(or (and (LexLocalId? fun) (symbol=? (LexLocalId-x fun) '___assertRaises))
                    (and (LexGlobalId? fun) (symbol=? (LexGlobalId-x fun) '___assertRaises)))
                (local [(define f (rec-desugar (second args)))
                        (define as (map rec-desugar (rest (rest args))))
                        (define exns (rec-desugar (first args)))
                        (define pass (rec-desugar (LexPass)))
                        (define fail (CApp (CId 'print (GlobalId))
                                           (list (make-builtin-str "Assert failure!"))
                                           (none)))
                        (define fail2 (CApp (CId 'print (GlobalId))
                                           (list (make-builtin-str "Assert failed!"))
                                           (none)))]
                  (CApp
                    (CFunc empty (none)
                           (CTryExceptElseFinally
                             (CApp f as (none))
                             (list
                               (CExcept (list exns) (none) pass)
                               (CExcept empty (none) fail))
                             fail2
                             pass)
                           false)
                    empty
                    (none)))]
               [(or (and (LexLocalId? fun) (symbol=? (LexLocalId-x fun) 'locals))
                    (and (LexGlobalId? fun) (symbol=? (LexGlobalId-x fun) 'locals)))
                (CBuiltinPrim '$locals empty)]
               [else
                 (local [(define f (rec-desugar fun))
                         (define f-expr f)
                         (define results (map rec-desugar args))]
                   (cond
                     [(CGetField? f-expr)
                      ;; FIX THIS
                      (local [(define o (CGetField-value f-expr))]
                        (CApp f-expr (cons o results) (none)))]
                     ; special case: "super" application gets extra 'self' argument
                     [(and (CId? f-expr) (symbol=? 'super (CId-x f-expr)))
                      (CApp f-expr (cons (CId 'self (LocalId)) results) (none))]
                     [else (CApp f-expr results (none))]))])]
      
      [LexAppStarArg (fun args sarg)
                     (local [(define f (rec-desugar fun))
                             (define results (map rec-desugar args))
                             (define s (rec-desugar sarg))]
                       (if (CGetField? f)
                           (local [(define o (CGetField-value f))]
                             (CApp f (cons o results) (some s)))
                           (CApp f results (some s))))]
      
      [LexClass (scp name bases body)
                (CClass name
                        (if (empty? bases)
                            (list 'object)
                            bases)
                        (desugar body))]

      [LexInstanceId (x ctx)
                     (error 'desugar "should not encounter an instance ID!")]

      [LexDotField (value attr) (CGetField (rec-desugar value) attr)]
      
      [LexTryExceptElseFinally (try excepts orelse finally)
                               (local [(define try-r (rec-desugar try))
                                       (define excepts-r (map rec-desugar excepts))
                                       (define orelse-r (rec-desugar orelse))
                                       (define finally-r (rec-desugar finally))]
                                 (CTryExceptElseFinally 
                                   try-r
                                   excepts-r
                                   orelse-r
                                   finally-r))]
      
      [LexExcept (types body) (CExcept (map rec-desugar types)
                                       (none)
                                       (rec-desugar body))]
      
      [LexWhile (test body orelse) (CWhile (rec-desugar test)
                                           (rec-desugar body)
                                           (rec-desugar orelse))]
      [LexFor (target iter body) (desugar-for target iter body)]
      
      [LexExceptAs (types name body)
                       (CExcept (map rec-desugar types) 
                                (some name)
                                (rec-desugar body))]
     
      ;; target is interpreted twice. FIX ME
      [LexAugAssign (op target value)
                    (local [(define target-r  target)
                            (define aug-r  (LexBinOp (context-load target) op value)) ]
                           (begin
                      (desugar (LexAssign (list target-r) (context-load aug-r)))))]
=======
      [PyApp (fun args)
             (local [(define f (rec-desugar fun global? env (none)))
                     (define f-expr (DResult-expr f))
                     (define-values (results last-env)
                       (map-desugar args global? (DResult-env f) (none)))]
               (DResult
                (CApp f-expr results (none))
                last-env))]
      
      [PyAppStarArg (fun args sarg)
                    (local [(define f (rec-desugar fun global? env (none)))
                            (define-values (results mid-env)
                              (map-desugar args global? (DResult-env f) (none)))
                            (define s (rec-desugar sarg global? mid-env (none)))]
                      (DResult
                       (CApp (DResult-expr f) results (some (DResult-expr s)))
                       (DResult-env s)))]
      
      [PyClass (name bases body)
               (local [(define newenv (get-globals/nonlocals body false env))
                       (define names (get-names body false newenv))
                       (define body-r (rec-desugar body false newenv (some name)))
                       (define modbody
                         (if (member '__init__ names)
                             (DResult-expr body-r)
                             (CSeq (DResult-expr body-r)
                                   (CAssign 
                                    (CId '__init__ (LocalId))
                                    (CFunc (list 'self) (none)
                                           (CAssign 
                                            (CGetField
                                             (CId 'self (LocalId))
                                             '__class__)
                                            (CBuiltinPrim '$class
                                                          (list (CId 'self (LocalId)))))
                                           (some name))))))
                     (define modenv 
                       (if (member '__init__ names)
                           (DResult-env body-r)
                           (add-id '__init__ (LocalId) (DResult-env body-r))))]
               (DResult
                 (CAssign (CId name (if global? (GlobalId) (LocalId)))
                          (CClass name
                                  (if (empty? bases)
                                    (list 'object)
                                    bases)
                                  modbody))
                 modenv))]
    
    [PyDotField (value attr)
                (local [(define value-r (rec-desugar value global? env (none)))]
                   (DResult
                     (CGetField (DResult-expr value-r) attr)
                     (DResult-env value-r)))]
      
      [PyTryExceptElseFinally (try excepts orelse finally)
                              (local [(define try-r (rec-desugar try global? env (none)))
                                      (define-values (excepts-r mid-env)
                                        (map-desugar excepts global? (DResult-env try-r) (none)))
                                      (define orelse-r (rec-desugar orelse global? mid-env (none)))
                                      (define finally-r (rec-desugar finally global?
                                                                     (DResult-env orelse-r) (none)))]
                                (DResult
                                 (CTryExceptElseFinally 
                                  (DResult-expr try-r)
                                  excepts-r
                                  (DResult-expr orelse-r)
                                  (DResult-expr finally-r))
                                 (DResult-env finally-r)))]
      
      [PyExcept (types body)
                (local [(define-values (types-r mid-env)
                          (map-desugar types global? env (none)))
                        (define body-r (rec-desugar body global? mid-env (none)))]
                  (DResult
                   (CExcept types-r 
                            (none)
                            (DResult-expr body-r))
                   (DResult-env body-r)))]
      
      [PyWhile (test body orelse)
               (local [(define test-r (rec-desugar test global? env (none)))
                       (define body-r (rec-desugar body global? (DResult-env test-r) (none)))
                       (define orelse-r (rec-desugar orelse global? (DResult-env body-r) (none)))]
                 (DResult 
                  (CWhile (DResult-expr test-r)
                          (DResult-expr body-r)
                          (DResult-expr orelse-r))
                  (DResult-env orelse-r)))]
      [PyFor (target iter body) (desugar-for target iter body global? env)]
      
      [PyExceptAs (types name body)
                  (local [(define-values (types-r mid-env)
                            (map-desugar types global? env (none)))
                          (define body-r (rec-desugar body global? mid-env (none)))]
                    (DResult
                     (CExcept types-r
                              (some name)
                              (DResult-expr body-r))
                     (DResult-env body-r)))]
      
      [PyAugAssign (op target value)
                   (local [(define target-r (rec-desugar target global? env (none)))
                           (define aug-r (rec-desugar (PyBinOp target op value)
                                                      global? (DResult-env target-r) (none)))]
                     (DResult
                      (CAssign (DResult-expr target-r)
                               (DResult-expr aug-r))
                      (DResult-env aug-r)))]
>>>>>>> master
      ; XXX: target is interpreted twice, independently.
      ; Is there any case where this might cause problems?
      ; TODO: this whole thing needs re-writing.  I'm just converting it to do a standard assignment. 
      
<<<<<<< HEAD
      [LexDelete (targets)
                 (let ([target (first targets)]) ; TODO: handle deletion of more than one target
                   (type-case LexExpr target
                     [LexSubscript (left ctx slice)
                                   (letrec ([desugared-target (rec-desugar left)]
                                            [desugared-slice (rec-desugar slice)]
                                            [target-id (new-id)]
                                            [target-var (CId target-id (LocalId))])
                                     (CLet target-id (LocalId) desugared-target
                                           (CApp (CGetField target-var
                                                            '__delitem__)
                                                 (list target-var
                                                       desugared-slice)
                                                 (none))))]
                     [else (error 'desugar "We don't know how to delete identifiers yet.")]))]
     
      [LexImport (names asnames) (rec-desugar (LexPass))]
                 ;(rec-desugar (desugar-import-py names asnames))]

      [LexImportFrom (module names asnames level) (rec-desugar (LexPass))]
                     ;(rec-desugar (desugar-importfrom-py module names asnames level))]
      [LexBuiltinPrim (s args) (CBuiltinPrim s (map desugar args))]
=======
      [PyDelete (targets)
                (let ([target (first targets)]) ; TODO: handle deletion of more than one target
                  (type-case PyExpr target
                    [PySubscript (left ctx slice)
                                 (letrec ([desugared-target (rec-desugar left global? env (none))]
                                          [desugared-slice
                                           (rec-desugar slice global? (DResult-env desugared-target) (none))]
                                          [target-id (new-id)])
                                   (DResult
                                    (CLet target-id (DResult-expr desugared-target)
                                          (CApp (CGetField (CId target-id (if global? (GlobalId) (LocalId)))
                                                           '__delitem__)
                                                (list (DResult-expr desugared-slice))
                                                (none)))
                                    (DResult-env desugared-slice)))]
                    [else (error 'desugar "We don't know how to delete identifiers yet.")]))]

      [PyImport (names asnames)
                (rec-desugar (desugar-import-py names asnames) global? env opt-class)]
        
      [PyImportFrom (module names asnames level)
                    (rec-desugar (desugar-importfrom-py module names asnames level) global? env opt-class)]
      )))

(define (desugar [expr : PyExpr]) : CExpr
  (type-case DesugarResult (rec-desugar expr true empty (none))
    [DResult (expr env) expr]))
>>>>>>> master


      [else
        (error 'desugar
               (string-append
                 "deprecation warning: deprecated lexical construct reached desugar: "
                 (to-string expr)))]

      )))

(define (context-load e)
  (type-case LexExpr e
    [LexSubscript (left context slice) (LexSubscript left 'Load slice)]
    [else e]))

(define (desugar [expr : LexExpr]) : CExpr
  (rec-desugar expr))
