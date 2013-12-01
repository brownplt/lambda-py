#lang plai-typed/untyped

(require "python-lexical-syntax.rkt"
         "python-core-syntax.rkt"
         "util.rkt"
         "builtins/type.rkt"
         "builtins/num.rkt"
         "python-syntax-operations.rkt"
         "builtins/str.rkt"
         "python-desugar-flags.rkt")

(require (typed-in racket/base (number->string : (number -> string)))
         (typed-in racket/list (last : ((listof 'a) -> 'a)))
         (typed-in racket/list (count : (('a -> boolean) (listof 'a) -> number)))
         (typed-in racket/base (append : ((listof 'a) (listof 'a) -> (listof 'a))))
         (typed-in racket (flatten : ((listof (listof 'a) ) -> (listof 'a)))))


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


(define (desugar-pymodule [es : (listof LexExpr)]) : CExpr
  (local [(define prelude (rec-desugar (LexPass)))
          (define body (rec-desugar (LexSeq es)))]
     (CModule prelude body)))


;; look through a  and find a list of all names from assignments and definition
;; if global scope, it only gets definitions, for local scope it gets
;; definitions and assignments

#|(define (desugar-pymodule [es : (listof PyExpr)] 
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
|#

(define (desugar-for [target : LexExpr] [iter : LexExpr]
                     [body : LexExpr] [orelse : LexExpr]) : CExpr
  (local [(define iter-id (new-id))]
    (rec-desugar
     (LexLocalLet iter-id (LexApp (LexGlobalId '%iter 'Load) (list iter) (list) (none) (none))
                  (LexWhile (LexBool true)
                            (LexSeq
                             (list
                              (LexTryExceptElse
                               (LexAssign (list target)
                                          (LexApp (LexDotField (LexLocalId iter-id 'Load)
                                                               '__next__)
                                                  (list) (list) (none) (none)))
                               (list (LexExcept (list (LexGlobalId 'StopIteration 'Load))
                                                (LexSeq (list orelse (LexBreak)))))
                               (LexPass))
                              body))
                            (LexPass))))))

(define (desugar-listcomp [body : LexExpr] [gens : (listof LexExpr)] ) : CExpr
  (local [(define list-id (new-id))
          (define (make-comploop gens)
            (cond
              [(empty? gens) (LexApp (LexDotField (LexLocalId list-id 'Load)
                                                  'append)
                                     (list body) (list) (none) (none))]
              [(cons? gens)
               (LexFor (LexComprehen-target (first gens))
                       (LexComprehen-iter (first gens))
                       (if (empty? (LexComprehen-ifs (first gens)))
                           (make-comploop (rest gens))
                           (LexIf (LexBoolOp 'And (LexComprehen-ifs (first gens)))
                                  (make-comploop (rest gens))
                                  (LexPass)))
                       (LexPass))]))
          (define full-expr
            (LexLocalLet list-id (LexList empty)
                         (LexSeq
                          (list
                           (make-comploop gens)
                           (LexLocalId list-id 'Load)))))]
    (rec-desugar full-expr)))

(define (desugar-generatorexp [body : LexExpr] [gens : (listof LexExpr)] ) : CExpr
  (local [(define (make-comploop gens)
            (cond
              [(empty? gens) (LexYield body)]
              [(cons? gens)
               (LexFor (LexComprehen-target (first gens))
                       (LexComprehen-iter (first gens))
                       (if (empty? (LexComprehen-ifs (first gens)))
                           (make-comploop (rest gens))
                           (LexIf (LexBoolOp 'And (LexComprehen-ifs (first gens)))
                                  (make-comploop (rest gens))
                                  (LexPass)))
                       (LexPass))]))
          (define gen-fun
            (CFunc empty (none) (rec-desugar (make-comploop gens)) (none)))]
    (CApp gen-fun empty (none))))

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

(define (desugar-excepts [exn-id : symbol] [excepts : (listof LexExpr)]) : CExpr
  (local [(define (rec-desugar-excepts es)
            (cond
              [(empty? es) ;; exceptions other than $Reraise are reraised if not catched
                (rec-desugar (LexIf (LexApp (LexGlobalId '%isinstance 'Load)
                                            (list (LexLocalId exn-id 'Load)
                                                  (LexGlobalId '$Reraise 'Load))
                                            (list) (none) (none))
                                    (LexPass)
                                    (LexRaise (LexLocalId exn-id 'Load))))]
              [else
                (local [(define except (first es))
                        (define-values (body as types)
                          (if (LexExcept? except)
                              (values
                                (rec-desugar (LexExcept-body except))
                                (none)
                                (LexExcept-types except))
                              (values
                                (rec-desugar (LexExceptAs-body except))
                                (some (LexExceptAs-name except))
                                (LexExceptAs-types except))))
                        (define checks 
                          (cond
                            [(empty? types)
                             (list
                               (LexApp (LexGlobalId '%isinstance 'Load)
                                       (list (LexLocalId exn-id 'Load)
                                             (LexGlobalId 'BaseException 'Load))
                                       (list) (none) (none)))]
                            [else (map (lambda (t)
                                         (LexApp (LexGlobalId '%isinstance 'Load)
                                                 (list (LexLocalId exn-id 'Load) t)
                                                 (list) (none) (none)))
                                       types)]))
                        (define condition (desugar-boolop 'Or checks))]
                  (CIf condition
                       (if (some? as)
                           (CLet (some-v as) (LocalId) (CId exn-id (LocalId)) body)
                           body)
                       (rec-desugar-excepts (rest es))))]))]
    (rec-desugar-excepts excepts)))

(define (id-to-symbol expr)
  (type-case LexExpr expr
    [LexLocalId (x ctx) x]
    [LexGlobalId (x ctx) x]
    [else (error 'desugar "cannot convert non-id to symbol with id-to-symbol")]))

;; helper for function desugar

(define (desugar-func [name : symbol] [args : (listof symbol)] [vararg : (optionof symbol)]
                      [kwonlyargs : (listof symbol)] [kwarg : (optionof symbol)]
                      [defaults : (listof LexExpr)] [kw_defaults : (listof LexExpr)]
                      [body : LexExpr] [opt-class : (optionof LexExpr)])

    ;; (cond
    ;;  ;; the "normal" case is receives different treatment to enable bootstrapping.
    ;;  [(and (empty? kwonlyargs) (none? kwarg) (empty? defaults) (empty? kw_defaults))
    ;;   (CFunc args vararg (rec-desugar body) (option-map id-to-symbol opt-class))]
    ;;  [else
    ;;   (CLet '$func (LocalId)
    ;;         ;; keyword-only and kwarg are appended to positional arguments,
    ;;         ;; ___nkwonlyargs and ___nkwarg attributes account for them.
    ;;         (CFunc (flatten (list args kwonlyargs (option->list kwarg))) vararg
    ;;                (rec-desugar body) (option-map id-to-symbol opt-class))
    ;;         (CSeq
    ;;          (CSetAttr (CId '$func (LocalId)) (make-builtin-str "___kwcall")
    ;;                    (CTrue)) ;; this functio has kw arguments and/or defaults
    ;;          (CSeq
    ;;           (CSetAttr (CId '$func (LocalId)) (make-builtin-str "___nkwonlyargs")
    ;;                     (make-builtin-num (length kwonlyargs)))
    ;;           (CSeq
    ;;            (CSetAttr (CId '$func (LocalId)) (make-builtin-str "___nkwarg")
    ;;                      (make-builtin-num (length (option->list kwarg))))
    ;;            (CSeq
    ;;             (CSetAttr (CId '$func (LocalId)) (make-builtin-str "__defaults__")
    ;;                       (CTuple (CId '%tuple (GlobalId)) (map rec-desugar defaults)))
    ;;             (CSeq
    ;;              (CSetAttr (CId '$func (LocalId)) (make-builtin-str "__kwdefaults__")
    ;;                        (CTuple (CId '%tuple (GlobalId)) (map rec-desugar kw_defaults)))
    ;;              (CId '$func (LocalId))))))))]))
  
  (local ((define desugared-body (rec-desugar body))
          (define option-maped-class (option-map id-to-symbol opt-class))
          (define (get-func-with-property [kwonlyargs_p : bool] [kwarg_p : bool])
            (cond
             [(or (and (eq? kwonlyargs_p false) (eq? kwarg_p false))
                  (and (empty? kwonlyargs) (none? kwarg) (empty? defaults) (empty? kw_defaults)))
              (CFunc args vararg desugared-body option-maped-class)]
             [else
              (CLet '$func (LocalId) (CFunc (flatten (list args
                                                           (if (eq? kwonlyargs_p true) kwonlyargs empty)
                                                           (if (eq? kwarg_p true) (option->list kwarg) empty)))
                                                     vararg desugared-body option-maped-class)
                    ;; keyword-only and kwarg are appended to positional arguments,
                    ;; ___nkwonlyargs and ___nkwarg attributes account for them.

                    (CSeq
                     (CSetAttr (CId '$func (LocalId)) (make-builtin-str "___kwcall")
                               (CTrue)) ;; this functio has kw arguments and/or defaults
                     (CSeq
                      (if (eq? kwonlyargs_p true)
                          (CSetAttr (CId '$func (LocalId)) (make-builtin-str "___nkwonlyargs")
                                    (make-builtin-num (length kwonlyargs)))
                          (CNone))
                      (CSeq
                       (if (eq? kwarg_p true)
                           (CSetAttr (CId '$func (LocalId)) (make-builtin-str "___nkwarg")
                                     (make-builtin-num (length (option->list kwarg))))
                           (CNone))
                       (CSeq
                        (if (eq? kwarg_p true)
                            (CSetAttr (CId '$func (LocalId)) (make-builtin-str "__defaults__")
                                      (CTuple (CId '%tuple (GlobalId)) (map rec-desugar defaults)))
                            (CNone))
                        (CSeq
                         (if (eq? kwonlyargs_p true)
                             (CSetAttr (CId '$func (LocalId)) (make-builtin-str "__kwdefaults__")
                                       (CTuple (CId '%tuple (GlobalId)) (map rec-desugar kw_defaults)))
                             (CNone))
                         (CId '$func (LocalId)))))
                      )))])))
         (get-func-with-property dsg-func-kwonlyargs dsg-func-kwarg)
         ))

;; desugar-with: based on the translation in http://www.python.org/dev/peps/pep-0343/
(define (desugar-with [context : LexExpr] [target : (optionof LexExpr)] [body : LexExpr])
  (let ([mgr (new-id)]
        [exit (new-id)]
        [value (new-id)]
        [exc (new-id)])
    (CLet mgr (LocalId) (rec-desugar context)
      ;; exit = type(mgr).__exit__
      (CLet exit (LocalId) (py-getfield (CBuiltinPrim '$class (list (CId mgr (LocalId))))
                                        '__exit__)
        ;; value = type(mgr).__enter__(mgr)
        (CLet value (LocalId) (py-app (py-getfield (CBuiltinPrim '$class (list (CId mgr (LocalId))))
                                                   '__enter__)
                                      (list (CId mgr (LocalId))) (none))
          (CLet exc (LocalId) (CTrue)
            (CTryFinally
             (CTryExceptElse
              (rec-desugar
               ;; target = value, only if as target is present
               ;; body
               (if (some? target)
                   (LexSeq
                    (list
                     (LexAssign (option->list target) (LexLocalId value 'Load))
                     body))
                   body))
              '$exc
              ;; except: the exceptional case is handled here
              (CSeq (CAssign (CId exc (LocalId)) (CFalse))
                    (CIf (py-app (CId exit (LocalId))
                                 (list (CId mgr (LocalId))
                                       (CBuiltinPrim '$class (list (CId '$exc (LocalId))))
                                       (CId '$exc (LocalId)) (CNone)) (none))
                         (CNone)
                         (CRaise (none))))
              (CNone))
             ;; finally: the no exception case is handled here
             (CIf (CId exc (LocalId))
                  (py-app (CId exit (LocalId))
                          (list (CId mgr (LocalId)) (CNone) (CNone) (CNone)) (none))
                  (CNone)))))))))

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
                                 (if (eq? dsg-subscript-assignment true)
                                     (py-app (py-getfield desugared-target '__setitem__)
                                             (list 
                                              desugared-slice
                                              desugared-value)
                                             (none))
                                     ;; if flag is off
                                     (CLet '$call (LocalId)
                                           (py-getfield desugared-target '__setitem__)
                                           (CApp (CBuiltinPrim 'obj-getattr (list (CId '$call (LocalId))
                                                                                  (make-builtin-str "__func__")))
                                                 (cons (CBuiltinPrim 'obj-getattr (list (CId '$call (LocalId))
                                                                                        (make-builtin-str "__self__")))
                                                       (list desugared-slice desugared-value))
                                                 (none)))
                                     ))]
                  ; An assignment to a tuple is desugared as multiple __setitem__ calls.
                  [LexTuple (vals)
                           (local [(define targets-r (map rec-desugar vals))
                                   (define value-r (rec-desugar value))
                                   (define assigns
                                     (if (eq? dsg-tuple-assignment true)
                                         (map2 (λ (t n) 
                                                  (CAssign t (py-app
                                                              (py-getfield (CId '$tuple_result (LocalId)) 
                                                                           '__getitem__)
                                                              (list (make-builtin-num n))
                                                              (none))))
                                               targets-r
                                               (build-list (length targets-r) identity))
                                         ;; if flag is false
                                         (map2 (λ (t n) 
                                                  (CAssign t (CLet '$call (LocalId)
                                                                   (py-getfield (CId '$tuple_result (LocalId)) 
                                                                                '__getitem__)
                                                                   (CApp (CBuiltinPrim 'obj-getattr (list (CId '$call (LocalId))
                                                                                                          (make-builtin-str "__func__")))
                                                                         (list (CBuiltinPrim 'obj-getattr (list (CId '$call (LocalId))
                                                                                                                (make-builtin-str "__self__")))
                                                                               (make-builtin-num n))
                                                                         (none)))))
                                               targets-r
                                               (build-list (length targets-r) identity))
                                         ))]
                             (CLet '$tuple_result (LocalId) value-r 
                                   (foldl (λ (a so-far) (CSeq so-far a))
                                          (first assigns) (rest assigns))))]
                  ; The others become a CAssign.
                  [else
                   ;; NOTE(joe): I think this was broken before for >1 target
                   ;; TODO(joe): Do this for >1 target, with the full tree walk on assignment
                   ;; and assuming an iterator on the right
                   (type-case LexExpr (first targets)
                     [LexDotField (obj fld)
                      (py-setfield (rec-desugar obj)
                                   fld
                                   (rec-desugar value))]
                     [else (CAssign (rec-desugar (first targets)) (rec-desugar value))])])]
                             
      [LexNum (n) (make-builtin-num n)]
      [LexSlice (lower upper step) (error 'desugar "Shouldn't desugar slice directly")]
      [LexBool (b) (if b (CId 'True (GlobalId)) (CId 'False (GlobalId)))]
      [LexNone () (CNone)]
      [LexStr (s) (make-builtin-str s)]
      [LexLocalId (x ctx) (CId x (LocalId))]
      [LexGlobalId (x ctx) (CId x (GlobalId))]
      [LexGlobals (ids body)
                  (local 
                   [(define (make-global-lets ids body)
                    (cond
                     [(empty? ids) (rec-desugar body)]
                     [else (CLet (first ids) (GlobalId)
                           (rec-desugar (LexUndefined))
                           (make-global-lets (rest ids) body))]))]
                   (make-global-lets ids body))]
      [LexLocalLet (x bind body) (CLet x (LocalId) (rec-desugar bind) (rec-desugar body))]
      ;hopefully this will come in handy for whatever we decide to do with locals
      ;right now it's just a stub.
      ;[LexInScopeLocals (ids) (rec-desugar (desugar-locals ids))]
      [LexUndefined () (CUndefined)]  
      [LexRaise (expr) (local [(define expr-r
                                 (if (or (LexLocalId? expr) (LexGlobalId? expr))
                                     ;;handle the implicit construction case
                                     (if (eq? dsg-raise true)
                                         (rec-desugar (LexApp expr (list) (list) (none) (none)))
                                        ; Why does CApp work here? it is supposed to be (simple-apply-method ...)
                                         (CLet '$call (LocalId) (rec-desugar expr)
                                               (simple-apply-method (py-getfield (CId '$call (LocalId)) '__call__) (list) ))) 
                                     (rec-desugar expr)))]
                         (CRaise 
                          (if (LexPass? expr)
                              (none)
                              (some expr-r))))]

      [LexYield (expr) (CYield (rec-desugar expr))]

      ;; assert check is always enabled, it doesn't test __debug__ builtin variable.
      [LexAssert (test msg)
                (rec-desugar
                 (LexIf test
                       (LexPass)
                       (LexRaise (LexApp (LexGlobalId 'AssertionError 'Load)
                                         msg (list) (none) (none)))))]

      ; LexPass is an empty lambda
      [LexPass () (CApp (CFunc empty (none) (CNone) (none)) empty (none))] 
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
                   ['Add (py-app (py-getfield left-c '__add__)
                               (list right-c)
                               (none))]
                   ['Sub (py-app (py-getfield left-c '__sub__)
                               (list right-c)
                               (none))]
                   ['Mult (py-app (py-getfield left-c '__mult__)
                                (list right-c)
                                (none))]
                   ['Pow (py-app (py-getfield left-c '__pow__)
                                 (list right-c)
                                 (none))]
                   ['Div (py-app (py-getfield left-c '__div__)
                               (list right-c)
                               (none))]
                   ['FloorDiv (py-app (py-getfield left-c '__floordiv__)
                                    (list right-c)
                                    (none))]
                   ['Mod (py-app (py-getfield left-c '__mod__)
                               (list right-c)
                               (none))]
                   ['BitAnd (py-app (py-getfield left-c '__and__)
                                  (list right-c)
                                  (none))]
                   ['BitOr (py-app (py-getfield left-c '__or__)
                                 (list right-c)
                                 (none))]
                   ['BitXor (py-app (py-getfield left-c '__xor__)
                                  (list right-c)
                                  (none))]
                   ['LShift (py-app (py-getfield left-c '__lshift__)
                                    (list right-c)
                                    (none))]
                   ['RShift (py-app (py-getfield left-c '__rshift__)
                                    (list right-c)
                                    (none))]
                   ['Eq (py-app (py-getfield left-c '__eq__)
                              (list right-c)
                              (none))]
                   ['Gt (py-app (py-getfield left-c '__gt__)
                              (list right-c)
                              (none))]
                   ['Lt (py-app (py-getfield left-c '__lt__)
                              (list right-c)
                              (none))]
                   ['LtE (py-app (py-getfield left-c '__lte__)
                               (list right-c)
                               (none))]
                   ['GtE (py-app (py-getfield left-c '__gte__)
                               (list right-c)
                               (none))]
                   ['NotEq (rec-desugar (LexUnaryOp 'Not (LexBinOp left 'Eq right)))]
                   ['In (CApp (CFunc (list 'container 'test) (none)
                                     (CLet '__infunc__ (LocalId)
                                           (py-getfield (CId 'container (LocalId))
                                                      '__in__)
                                           (CIf (CId '__infunc__ (LocalId))
                                                (CReturn
                                                  (py-app
                                                    (CId '__infunc__ (LocalId))
                                                    (list 
                                                          (CId 'test (LocalId)))
                                                    (none)))
                                                (CRaise (some
                                                  (make-exception 'TypeError
                                                                  (string-append
                                                                   "argument of type '___'"
                                                                   "is not iterable"))))))
                                     (none))
                              (list right-c left-c)
                              (none))]
                   ['NotIn (rec-desugar (LexUnaryOp 'Not (LexBinOp left 'In right)))]
                   [else (CBuiltinPrim op (list left-c right-c))]))]

      [LexUnaryOp (op operand)
                  (case op
                    ['Not (CIf (py-app (CId '%bool (GlobalId)) (list (desugar operand)) (none)) (CId 'False (GlobalId)) (CId 'True (GlobalId)))]
                    ['USub (rec-desugar (LexBinOp (LexNum 0) 'Sub operand))]
                    ['UAdd (rec-desugar (LexBinOp (LexNum 0) 'Add operand))]
                    ['Invert (local [(define roperand (rec-desugar operand))]
                               (py-app (py-getfield roperand '__invrt__)
                                     (list)
                                     (none)))]
                    [else (CBuiltinPrim op (list (rec-desugar operand)))])]
      
      [LexBoolOp (op values) (desugar-boolop op values)]
      [LexCompOp (l op rights) (desugar-compop l op rights)]
      [LexListComp (elt gens) (desugar-listcomp elt gens)]
      [LexGeneratorExp (elt gens) (desugar-generatorexp elt gens)]
      [LexComprehen (target iter ifs) (error 'desugar "Can't desugar LexComprehen")]
      
      [LexLam (args vararg kwonlyargs kwarg defaults kw_defaults body)
              (desugar-func 'lambda args vararg kwonlyargs kwarg defaults kw_defaults
                            (LexReturn (some body)) (none))]

      [LexFunc (name args vararg kwonlyargs kwarg defaults kw_defaults body decorators opt-class)
               (cond
                [(empty? decorators)
                 ;; no decorators, desugar function
                 (desugar-func name args vararg kwonlyargs kwarg defaults kw_defaults
                               body opt-class)]
                [else
                 ;; first apply decorators to the function
                 (rec-desugar
                  (foldr (lambda (decorator func) (LexApp decorator (list func) (list) (none) (none)))
                         (LexFunc name args vararg kwonlyargs kwarg defaults kw_defaults body (list) opt-class)
                         decorators))])]

      [LexReturn (value) (CReturn (type-case (optionof CExpr) (option-map rec-desugar value)
                                    [some (v) v]
                                    [none () (CNone)]))]
      
      [LexDict (keys values)
       (local [
        (define (pairs->tupleargs keys values)
          (cond
            [(empty? keys) empty]
            [(cons? keys)
             (cons (CTuple (CId '%tuple (GlobalId))
                           (list (rec-desugar (first keys))
                                 (rec-desugar (first values))))
                   (pairs->tupleargs (rest keys) (rest values)))]))
        ]
         (if (eq? dsg-dict true)
             (py-app (CId '%dict (GlobalId))
                     (list
                      (CList (CId '%list (GlobalId))
                             (pairs->tupleargs keys values)))
                     (none))
             (simple-apply-method (py-getfield  (CId '%dict (GlobalId)) '__call__)
                                  (list (CList (CId '%list (GlobalId))
                                               (pairs->tupleargs keys values))))
             ))]
      [LexSet (elts)
              (CSet (CId '%set (GlobalId)) (map rec-desugar elts))]
      [LexList (values)
               (CList (CId '%list (GlobalId)) (map rec-desugar values))]
      [LexTuple (values)
                (CTuple (CId '%tuple (GlobalId)) (map rec-desugar values))]
      
      [LexSubscript (left ctx slice)
                    (cond
                     [(and (symbol=? ctx 'Load) (eq? dsg-subscript true)) ;; flag is true
                      (local [(define left-id (new-id))
                              (define left-var (CId left-id (LocalId)))
                              (define left-r (rec-desugar left))]
                        (if (LexSlice? slice)
                            (local [(define slice-low (rec-desugar (LexSlice-lower slice)))
                                    (define slice-up (rec-desugar (LexSlice-upper slice)))
                                    (define slice-step (rec-desugar (LexSlice-step slice)))]
                              (CLet left-id
                                    (LocalId)
                                    left-r
                                    (py-app (py-getfield left-var
                                                         '__slice__)

                                            (list slice-low
                                                  slice-up slice-step)
                                            (none))))
                            (local [(define slice-r (rec-desugar slice))
                                    (define exn-id (new-id))] 
                              (CLet left-id
                                    (LocalId)
                                    left-r
                                    (CSeq
                                     (CTryExceptElse
                                      (py-getfield (CId left-id (LocalId))
                                                   '__getitem__)
                                      exn-id
                                      (default-except-handler
                                        exn-id
                                        (CRaise (some (make-exception 
                                                       'TypeError
                                                       "object is not subscriptable"))))
                                      (CNone))
                                     (py-app (py-getfield (CId left-id (LocalId))
                                                          '__getitem__)
                                             (list slice-r)
                                             (none) ;TODO: not sure what to do with stararg.
                                             ))))))]
                     [(symbol=? ctx 'Load) ;; flag is false
                      (local [(define left-id (new-id))
                              (define left-var (CId left-id (LocalId)))
                              (define left-r (rec-desugar left))]
                        (if (LexSlice? slice)
                            (local [(define slice-low (rec-desugar (LexSlice-lower slice)))
                                    (define slice-up  (rec-desugar (LexSlice-upper slice)))
                                    (define slice-step (rec-desugar (LexSlice-step slice)))]
                              (CLet left-id (LocalId) left-r
                                    (simple-apply-method (py-getfield left-var '__slice__)
                                                         (list slice-low slice-up slice-step))))
                            (let ((slice-r (rec-desugar slice)))
                              (CLet left-id (LocalId) left-r
                                    (simple-apply-method (py-getfield left-var '__getitem__)
                                                         (list slice-r))))))
                      ]
                     [(symbol=? ctx 'Store)
                      (error 'desugar "bad syntax: LexSubscript has context 'Store' outside a LexAssign")]
                     [else (error 'desugar "unrecognized context in LexSubscript")])]
      
      [LexBreak () (CBreak)]

      [LexContinue () (CContinue)]

      [LexApp (fun args keywords stararg kwarg)
              (py-app-kw (rec-desugar fun) (map rec-desugar args) (map rec-desugar keywords)
                         (option-map rec-desugar stararg) (option-map rec-desugar kwarg))]

      [LexClass (scp name bases body keywords stararg kwarg decorators)
                (cond
                  [(empty? decorators)
                   ;; no decorators, desugar class
                   (let* ([scope (type-case LocalOrGlobal scp
                                   [Locally-scoped () (LocalId)]
                                   [Globally-scoped () (GlobalId)]
                                   [else (error 'expr "should be no more instance scope!")])]
                          [bases-list (if (empty? bases)
                                          (list (CId '%object (GlobalId)))
                                          (map desugar bases))]
                          [base-id (new-id)]
                          ;; (CNone) is because we may not have a tuple class object yet, type-uniqbases fixes it.
                          [bases-tuple (CTuple (CNone) (cons (CId base-id (LocalId)) (rest bases-list)))]
                          [new-class (make-class scope name bases-tuple (desugar body))]
                          [call-metaclass (CApp (CId '%call_metaclass (GlobalId))
                                                (list (make-builtin-str (symbol->string name))
                                                      (CBuiltinPrim 'type-uniqbases (list bases-tuple))
                                                      new-class
                                                      (CTuple (CId '%tuple (GlobalId)) (map rec-desugar keywords))
                                                      (CTuple (CId '%tuple (GlobalId)) (map rec-desugar (option->list stararg)))
                                                      (CTuple (CId '%tuple (GlobalId)) (map rec-desugar (option->list kwarg))))
                                                (none))])
                     (CLet base-id (LocalId) (first bases-list)
                           (if (and (empty? keywords) (none? kwarg))
                               (CIf (CBuiltinPrim 'type-metaclass (list (first bases-list)))
                                    call-metaclass
                                    new-class)
                               call-metaclass)))]
                  [else
                   ;; first apply decorators to the class
                   (rec-desugar
                    (foldr (lambda (decorator class) (LexApp decorator (list class) (list) (none) (none)))
                           (LexClass scp name bases body keywords stararg kwarg empty)
                           decorators))])]

      [LexInstanceId (x ctx)
                     (error 'desugar "should not encounter an instance ID!")]

      [LexDotField (value attr) (py-getfield (rec-desugar value) attr)]
      [LexExprField (value attr) 
        (if (eq? dsg-expr-field true)
            (CGetAttr (rec-desugar value) (rec-desugar attr))
            (CNone))]
      [LexExprAssign (obj attr value)
        (if (eq? dsg-expr-assign true)
            (CSetAttr (rec-desugar obj) (rec-desugar attr) (rec-desugar value))
            (CSetAttr (rec-desugar obj) (rec-desugar attr) (CNone)))]

      [LexTryExceptElse (try excepts orelse)
                        (if (eq? dsg-try-except-else true)
                            (local [(define try-r (rec-desugar try))
                                    (define exn-id (new-id))
                                    (define excepts-r (desugar-excepts exn-id excepts))
                                    (define orelse-r (rec-desugar orelse))]
                                   (CTryExceptElse 
                                    try-r
                                    exn-id
                                    excepts-r
                                    orelse-r))
                            (local [(define except (first excepts))
                                    (define body (if (LexExcept? except)
                                                     (LexExcept-body except)
                                                     (LexExceptAs-body except)))]
                                   (CTryExceptElse (rec-desugar try) (new-id) (rec-desugar body) (rec-desugar orelse))))]

      [LexTryFinally (try finally)
                     (if (eq? dsg-try-finally true)
                         (local [(define try-r (rec-desugar try))
                                 (define finally-r (rec-desugar finally))]
                                (CTryFinally
                                 try-r
                                 finally-r))
                         (CTryFinally (CNone) (CNone)))]

      [LexExcept (types body) (error 'desugar "should not encounter LexExcept!")]
      [LexExceptAs (types name body) (error 'desugar "should not encounter LexExcept!")]

      [LexWith (context target body)
               (if (eq? dsg-with true)
                   (desugar-with context target body)
                   (CTryFinally (CNone) (CNone)))]

      [LexWhile (test body orelse) 
                (if (eq? dsg-while true)
                    (CWhile (rec-desugar test)
                            (rec-desugar body)
                            (rec-desugar orelse))
                    (CWhile (CFalse) (CNone) (CNone)))]

      [LexFor (target iter body orelse)
              (if (eq? dsg-for true)
                  (desugar-for target iter body orelse)
                  (CWhile (CFalse) (CNone) (CNone)))]

      ;; target is interpreted twice. FIX ME
      [LexAugAssign (op target value)
                    (local [(define target-r  target)
                            (define aug-r  (LexBinOp (context-load target) op value)) ]
                      (if (eq? dsg-augassignment true)
                          (desugar (LexAssign (list target-r) (context-load aug-r)))
                          (CNone)))]
      ; XXX: target is interpreted twice, independently.
      ; Is there any case where this might cause problems?
      ; TODO: this whole thing needs re-writing.  I'm just converting it to do a standard assignment. 
      
      [LexDelete (targets)
                 (local
                  [(define (handle-delete target)
                     (type-case LexExpr target
                       [LexSubscript (left ctx slice)
                                     (letrec ([desugared-target (rec-desugar left)]
                                              [desugared-slice (rec-desugar slice)]
                                              [target-id (new-id)]
                                              [target-var (CId target-id (LocalId))])
                                       (CLet target-id (LocalId) desugared-target
                                             (py-app (py-getfield target-var
                                                              '__delitem__)
                                                   (list 
                                                    desugared-slice)
                                                   (none))))]
                       [LexLocalId (x ctx) (rec-desugar
                                            (LexAssign (list (LexLocalId x ctx)) (LexUndefined)))]
                       [LexGlobalId (x ctx) (rec-desugar
                                            (LexAssign (list (LexGlobalId x ctx)) (LexUndefined)))]
                       [LexDotField (value attr) (py-delfield (rec-desugar value) attr)]
                       [else (error 'desugar (string-append "We don't know how to delete this yet: " (to-string target)))]))
                  (define (make-sequence [exprs : (listof CExpr)] )
                     (cond
                      [(empty? exprs) (error 'make-sequence "went too far")]
                      [(empty? (rest exprs)) (first exprs)]
                      [else (CSeq (first exprs) (make-sequence (rest exprs)))]))]
                  (if (eq? dsg-delete true)
                      (make-sequence (map handle-delete targets))
                      (CNone)))]
      
      [LexBuiltinPrim (s args)
         (if (eq? dsg-built-in-prims true)
             (CBuiltinPrim s (map desugar args))
             (CBuiltinPrim s (list)))]
      [LexCore (e) e]
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
