#lang plai-typed/untyped

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "builtins/object.rkt"
         "builtins/bool.rkt"
         "builtins/tuple.rkt"
         "builtins/num.rkt"
         "builtins/dict.rkt"
         "builtins/method.rkt"
         "util.rkt"
         (typed-in "python-lib.rkt" (python-lib : ('a -> 'b)))
         (typed-in racket/base (hash-copy : ((hashof 'a 'b) -> (hashof 'a 'b))))
         (typed-in racket/base (hash-map : ((hashof 'a 'b) ('a 'b -> 'c) -> (listof 'c))))
         (typed-in racket/base (hash-count : ((hashof 'a 'b) -> number)))
         (typed-in racket/string (string-join : ((listof string) string -> string)))
         (typed-in racket/base (raise-user-error : (string -> 'a)))
         (typed-in racket/base (hash->list : ((hashof 'a 'b)  -> (listof 'c))))
         (typed-in racket/base (car : (('a * 'b) -> 'a)))
         (typed-in racket/base (cdr : (('a * 'b) -> 'b)))
         (typed-in racket/list (last : ((listof 'a) -> 'a)))
         (typed-in racket/base (append : ((listof 'a) (listof 'a) -> (listof'a))))
         (typed-in racket/list (remove-duplicates : ((listof 'a) -> (listof 'a))))
         (typed-in racket/base (immutable? : ((hashof 'a 'b) -> boolean)))
         (typed-in racket/base (format : (string 'a -> string)))
         (typed-in racket/base (substring : (string number number -> string)))
         (typed-in racket/base (min : (number number -> number)))
         (typed-in racket/base (string-length : (string -> number)))

         )

(define (handle-result result fun)
   (type-case Result result
     [v*s (v s) (fun v s)]
     [Return (v s) (return-exception s)]
     [Break (s) (break-exception s)]
     [Continue (s) (continue-exception s)] 
     [Exception (v s) (Exception v s)]))

(define (append3 a b c)
  (append a (append b c)))

;; interp-cascade, interprets a list of expressions with an initial store,
;; environment and produces a ResultList, which either contains the final
;; store and list of results, or a single Abnormal result
(define (interp-cascade [exprs : (listof CExpr)] 
                        [init-s : Store]
                        [env : Env]
                        [stk : Stack]) : ResultList
  (cond
    [(empty? exprs) (v*s/list empty init-s)]
    [(cons? exprs)
     (let ([first-result (interp-env (first exprs) env init-s stk)])
       (type-case Result first-result
         [v*s (vfr sfr)
          (let ([rest-result (interp-cascade (rest exprs) sfr env stk)])
            (type-case ResultList rest-result
             [v*s/list (rs sf) (v*s/list (cons first-result rs) sf)]
             [Abnormal (r) rest-result]))]
         [else (Abnormal first-result)]))]))

;; common code to interpret function and method application, first argument must be a VClosure.
(define (interp-vclosure [vfun : CVal] [arges : (listof CExpr)] 
                         [stararg : (optionof CExpr)]
                         [env : Env] [sfun : Store] [stk : Stack]) : Result
  (type-case CVal vfun
    [VClosure (cenv argxs vararg body opt-class)
              (type-case ResultList (interp-cascade arges sfun env stk)
                [Abnormal (r) r]
                [v*s/list (argvs-r sc)
                 (local [(define result
                           (if (some? stararg)
                               (local [(define sarg-r
                                         (interp-env (some-v stararg) env sc stk))
                                       ;; todo: support other types
                                       ;; for star args
                                       (define l (MetaTuple-v 
                                                   (some-v 
                                                     (VObjectClass-mval 
                                                       (fetch-ptr (v*s-v sarg-r) (v*s-s sarg-r))))))]
(begin ;(display (format "applying: ~a\n" vfun))
       ;(display (format "starargs: ~a\n" (map (lambda (p) (fetch-ptr p (v*s-s sarg-r))) l)))
                                 (bind-and-execute 
                                   body opt-class argxs vararg 
                                   (append argvs-r (map (lambda (v)
                                                        (v*s v (v*s-s sarg-r)))
                                                      l))
                                   (append arges (map (lambda(x)
                                                        (make-builtin-num 0))
                                                      l))
                                   env cenv (v*s-s sarg-r) stk))) 
                               (bind-and-execute
                                 body opt-class argxs vararg
                                 argvs-r arges env
                                 cenv sc stk)))]
                   (type-case Result result
                     [v*s (vb sb) (alloc-result vnone sb)]
                     [Return (vb sb) (v*s vb sb)]
                     [Break (sb) (break-exception sb)]
                     [Continue (sb) (continue-exception sb)]
                     [Exception (vb sb) (Exception vb sb)]))])]
    [else (error 'interp (string-append "Not a closure: " (to-string vfun)))]))

(define (interp-capp [fun : CExpr] [arges : (listof CExpr)] 
                     [stararg : (optionof CExpr)]
                     [env : Env] [sto : Store] [stk : Stack]) : Result
  (begin ;(display "APP: ") (display fun) (display "\n") (display arges) (display "\n\n\n")
         ;(display env) (display "\n\n")
 (handle-result (interp-env fun env sto stk)
  (lambda (vfun-ptr sfun)
    (let ([vfun (fetch-ptr vfun-ptr sfun)])
    (type-case CVal vfun
      [VClosure (cenv argxs vararg body opt-class)
                (interp-vclosure vfun arges stararg env sfun stk)]

      [VObjectClass (b mval d class)
                  ;; This means we're calling an object, so apply its __call__ method.
                  ;; Class calls are now handled by type.__call__ method (Alejandro).
                  (local [(define __call__ (get-field '__call__ vfun-ptr env sfun))]
                    (type-case Result __call__
                      [v*s (vc sc)
                      (begin ;(display (format "Calling ~a\n" (fetch-ptr vc sc)))
                           (cond
                             ;; for bound methods use __func__ attribute and __self__
                             [(and (is-obj-ptr? vc sc) (equal? (VObjectClass-antecedent (fetch-ptr vc sc)) 'method))
                              (local 
                                [(define func
                                   (fetch-ptr
				     (fetch-once
				       (some-v (hash-ref (VObjectClass-dict (fetch-ptr vc sc)) '__func__)) sc) sc))
                                 (define w_self (hash-ref (VObjectClass-dict (fetch-ptr vc sc)) '__self__))
                                 (define id_self (new-id))
                                 (define m_arges (cons (CId id_self (LocalId)) arges))
                                 ;; extend the environment with self to support self aliasing
                                 (define m_env
                                   (cons (hash-set (first env) id_self (some-v w_self)) 
                                         (rest env)))]
                                (begin
                                  ;(display (format "Method is: ~a" vc)) (display "\n")
                                  ;(display (format "Func is: ~a" func)) (display "\n")
                                  (interp-vclosure func m_arges stararg m_env sc stk)))]
                             [else
                               ;; for unbound methods, use function application
                               (interp-vclosure (fetch-ptr vc sc) arges stararg env sc stk)]))]
                      [Return (vfun sfun) (return-exception sfun)]
                      [Break (sfun) (break-exception sfun)]
                      [Continue (sfun) (continue-exception sfun)]
                      [Exception (vfun sfun) (mk-exception 'TypeError
                                                           (string-append 
                                                             (symbol->string b)
                                                             " object is not callable")
                                                           sto)]))]
      [else (error 'interp "Not a closure or constructor.")]))))))

(define (interp-while [test : CExpr] [body : CExpr] [orelse : CExpr]
                      [env : Env] [sto : Store] [stk : Stack]) : Result
  (local [(define test-r (interp-env test env sto stk))]
    ;; if test results in an exception, pass it along
    (if (Exception? test-r)
        test-r
        (if (truthy? (v*s-v test-r) (v*s-s test-r))
            (local [(define body-r (interp-env body env (v*s-s test-r) stk))]
              (cond
                ;; if the body results in an exception of return, pass it along
                [(or (Exception? body-r) (Return? body-r)) body-r]
                ;; if it results in a break, return None
                [(Break? body-r) (alloc-result vnone (Break-s body-r))]
                ;; if it resulted in a value or continue, attempt to run the loop again
                [else (interp-while test body orelse env
                                    (if (v*s? body-r)
                                        (v*s-s body-r)
                                        (Continue-s body-r))
                                    stk)]))
            (interp-env orelse env (v*s-s test-r) stk)))))

;; bind-and-execute, binds the arguments in the closure's
;; environment and then applies the closure.
(define (bind-and-execute [body : CExpr]
                          [opt-class : (optionof symbol)]
                          [argxs : (listof symbol)]
                          [vararg : (optionof symbol)] [argvs : (listof Result)]
                          [arges : (listof CExpr)] [env : Env]
                          [ext : Env] [sto : Store] [stk : Stack]) : Result
  (local
    [(define-values (env_new sto_new result) (bind-args argxs vararg argvs arges env ext sto))]
    (if (some? result)
        (some-v result)
        (local [(define class 
                  (if (some? opt-class)
                      ;; fetch class using the closure's environment
                      (some (fetch-once (some-v (lookup (some-v opt-class) ext)) sto))
                      (none)))
                (define self 
                  (if (and (some? opt-class) (> (length argvs) 0))
                      ;; self is the first argument, if any, for methods
                      (some (v*s-v (first argvs)))
                      (none)))]
        (interp-env body env_new sto_new 
                    ;; push new activation record on the stack
                    ;; used the dynamic environment for compatibility with base code.
                    (cons (Frame env class self) stk))))))

(define (interp-let [name : symbol] [type : IdType]
                    [val : CVal] [sto : Store]
                    [body : CExpr] [env : Env] [stk : Stack]) : Result
  (local [(define loc (new-loc))
          (define newenv (cons (hash-set (first env) name loc) (rest env)))]
    (interp-env body newenv (hash-set sto loc val) stk)))

;; interp-id will first lookup id in env, then fetch the value of the id in the sto.
(define (interp-id [id : symbol] [type : IdType]
                   [env : Env] [sto : Store]) : Result
  (local [(define name-error-str
            (string-append "name '"
                                  (string-append (symbol->string id)
                                                 "' is not defined")))
          (define freevar-error-str
            (string-append "free variable '"
                           (string-append (symbol->string id)
                                          "' referenced before assignment in enclosing scope")))
          (define unboundlocal-error-str
            (string-append "local variable '"
                           (string-append (symbol->string id)
                                          "' referenced before assignment")))]
    (type-case IdType type
      [LocalId () 
               (local [(define local-w (lookup-local id env))]
                 (begin ;(display "Local ") (display id) (display " = ") (display local-w)
                 (if (some? local-w)
                     (local [(define full-val (fetch-once (some-v local-w) sto))]
                      (begin ;(display "\nValue is: ") (display full-val) (display "\n\n")
                       (type-case CVal full-val
                         [VUndefined () (mk-exception 'UnboundLocalError
                                                      unboundlocal-error-str
                                                      sto)]
                         [else (v*s full-val sto)])))
                     (local [(define full-w (lookup id env))]
                       (if (some? full-w)
                           (local [(define full-val (fetch-once (some-v full-w) sto))]
                             (type-case CVal full-val
                               [VUndefined () (mk-exception 'NameError freevar-error-str sto)]
                               [else (v*s full-val sto)]))
                           (mk-exception 'NameError
                                         (string-append "global " name-error-str)
                                         sto))))))]
      [GlobalId ()
                (local [(define full-w (lookup-global id env))]
                 (begin ;(display "Global ") (display id) (display " = ") (display full-w)
                  (if (some? full-w)
                      (local [(define full-val (fetch-once (some-v full-w) sto))]
                        (type-case CVal full-val
                          [VUndefined () (mk-exception 'NameError name-error-str sto)]
                          [else (v*s full-val sto)]))
                      (mk-exception 'NameError name-error-str sto))))])))

;; interp-env : CExpr * Env * Store * Stack -> Result
(define (interp-env [expr : CExpr] [env : Env] [sto : Store] [stk : Stack]) : Result
  (begin ;(display expr) (display "\n")
         ;(display env) (display "\n\n")
  (type-case CExpr expr
    [CModule (prelude body)
             (local [(define prelude-r (interp-env prelude env sto stk))]
                (handle-result prelude-r
                  (lambda (v s) (interp-env body env s stk))))]
    
    [CTrue () (renew-true env sto)]
    [CFalse () (renew-false env sto)]
    [CNone () (alloc-result vnone sto)]
    [CUndefined () (v*s (VUndefined) sto)]

    [CClass (name bases body)
            (begin ;(display "BEGIN CLASS\n") (display bases)
            (handle-result (interp-env bases env sto stk)
              (lambda (vbases sbases)
                   (handle-result (interp-env body (cons (hash empty) env) sbases stk)
                     (lambda (vbody sbody)
                          (begin ;(display name) (display "\n")
                                 ;(display env) (display "\n")
                                 ;(display sbody) (display "\n")
                                 (let ([res (mk-type name vbases (hash empty) sbody env)])
                                   res)))))))]
   
    [CGetField (value attr)
    (begin
      ;(display "Getting field ") (display attr) (display "from: \n") (display value)
      ;(display "\n\n")
               (handle-result (interp-env value env sto stk)
                          (lambda (vval sval) (get-field attr vval env sval))))]
			
    [CSeq (e1 e2) (type-case Result (interp-env e1 env sto stk)
                    [v*s (v1 s1) (interp-env e2 env s1 stk)]
                    [Return (v1 s1) (Return v1 s1)]
                    [Break (s1) (Break s1)]
                    [Continue (s1) (Continue s1)] 
                    [Exception (v1 s1) (Exception v1 s1)])]
    
    ;; note that for now we're assuming that dict keys and values aren't going
    ;; to mess with the environment and store, but this might be wrong
    [CDict (class contents)
     (local [
       (define (interp-pair p sto)
         (handle-result (interp-env (car p) env sto stk)
           (lambda (carv cars)
             (handle-result (interp-env (cdr p) env cars stk)
               (lambda (cdrv cdrs)
                 (vpair*s carv cdrv cdrs))))))
       (define dict-hash (make-hash empty))
       (define (interp-pairs lst sto)
         (cond
           [(empty? lst) sto]
           [(cons? lst)
            (type-case ResultPair (interp-pair (first lst) sto)
             [vpair*s (v1 v2 new-sto)
               (begin
                 (hash-set! dict-hash v1 v2)
                 (interp-pairs (rest lst) new-sto))])]))
       (define post-dict-sto (interp-pairs (hash->list contents) sto))]
     (handle-result (interp-env class env post-dict-sto stk)
      (lambda (cval csto)
       (alloc-result (VObjectClass 'dict
                       (some (MetaDict dict-hash))
                       (hash empty)
                       (some cval))
                     csto))))]

    [CSet (class values)
     (type-case ResultList (interp-cascade values sto env stk)
      [Abnormal (r) r]
      [v*s/list (result-list new-s)
       (handle-result (interp-env class env new-s stk)
        (lambda (cval csto)
         (let ([val-list (map v*s-v result-list)])
          (alloc-result (VObjectClass 'set
                        (some (MetaSet (make-set val-list)))
                        (hash empty)
                        (some cval))
               csto))))])]
    
    [CList (class values)
     (type-case ResultList (interp-cascade values sto env stk)
      [Abnormal (r) r]
      [v*s/list (result-list new-s)
       (handle-result (interp-env class env new-s stk)
        (lambda (cval csto)
         (let ([val-list (map v*s-v result-list)])
          (alloc-result (VObjectClass 'list
                       (some (MetaList val-list))
                       (hash empty)
                       (some cval))
              csto))))])]

    [CTuple (class values)
     (type-case ResultList (interp-cascade values sto env stk)
      [Abnormal (r) r]
      [v*s/list (result-list new-s)
       (handle-result (interp-env class env new-s stk)
        (lambda (cval csto)
         (let ([val-list (map v*s-v result-list)])
          (alloc-result (VObjectClass 'tuple
                       (some (MetaTuple val-list))
                       (hash empty)
                       (some cval))
              csto))))])]

    [CAssign (t v) 
             (begin ;(display "\nASSIGN: ") (display t) (display " | ") (display v) (display "\n")
               (handle-result (interp-env v env sto stk)
                 (lambda (vv sv)
                      (type-case CExpr t
                        [CId (x type) (assign-to-id t vv env sv)]
                        [CGetField (o a) (assign-to-field o a vv env sv stk)]
                        [else (mk-exception 'SyntaxError
                                            "can't assign to literals"
                                            sv)]))))]
    
    [CIf (i t e) (handle-result (interp-env i env sto stk)
                   (lambda (vi si) (if (truthy? vi si)
                                       (interp-env t env si stk)
                                       (interp-env e env si stk))))]
    
    [CId (x t) (interp-id x t env sto)]

    [CObject (class mval)
             (handle-result (interp-env class env sto stk)
               (lambda (cval csto)
                 (alloc-result (VObjectClass (MetaClass-c (some-v (VObjectClass-mval (fetch-ptr cval csto))))
                                    mval
                                    (hash empty)
                                    (some cval))
                      csto)))]

    [CLet (x type bind body)
          (begin ;(display "LET: ") (display x) (display " ")
                 ;(display type) (display bind) (display "\n")
          (handle-result (interp-env bind env sto stk)
            (lambda (val sto)
              (interp-let x type val sto body env stk))))]

    [CApp (fun arges sarg)
          (begin ;(display "CApp") (display fun) (display arges) (display "\n")
          (interp-capp fun arges
                       (if (none? sarg)
                           (some (CTuple (CNone) empty))
                           sarg)
                       env sto stk))]

    [CFunc (args sargs body opt-class) 
           (begin ;(display "func ") (display env) (display "\n\n")
           (alloc-result (VClosure
                  (cons (hash empty) env)
                  ;(if (some? opt-class) (rest env) env)
                  args sargs body opt-class)
                sto))]

    [CReturn (value)
             (handle-result (interp-env value env sto stk)
               (lambda (vv sv) (Return vv sv)))]

    [CPrim1 (prim arg) 
            (handle-result (interp-env arg env sto stk)
              (lambda (varg sarg)
                   (case prim
                     ['Not (if (truthy? varg sarg)
                             (v*s false-val sarg)
                             (v*s true-val sarg))]
                     [else (v*s (python-prim1 prim (fetch-ptr varg sarg)) sarg)])))]

    [CWhile (body test orelse) (interp-while body test orelse env sto stk)]

    [CPrim2 (prim arg1 arg2) (interp-cprim2 prim arg1 arg2 sto env stk)]
    
    [CBuiltinPrim (op args) 
                  (type-case ResultList (interp-cascade args sto env stk)
                   [Abnormal (r) r]
                   [v*s/list (result-list new-s)
                    (local [(define val-list (map v*s-v result-list))]
                            (builtin-prim op val-list env new-s stk))])]
    [CRaise (expr) 
            (if (some? expr)
                (handle-result (interp-env (some-v expr) env sto stk)
                  (lambda (vexpr sexpr)
                     (begin
                       ;(display "Raising: ") (display vexpr)
                       ;(display "\nValue is: ") (display (fetch-ptr vexpr sexpr))
                       ;(display "\n\n")
                       (cond
                         [(and (is-obj-ptr? vexpr sexpr)
                               (object-is? (fetch-ptr vexpr sexpr) 'BaseException env sexpr))
                          (Exception vexpr sexpr)]
                         [else (mk-exception 'TypeError
                                             "exceptions must derive from BaseException"
                                             sexpr)]))))
                (mk-exception '$Reraise
                              "reraise previous exception if possible"
                              sto))]
    
    [CTryExceptElse (try exn-id excepts orelse)
         (type-case Result (interp-env try env sto stk)
            [v*s (vtry stry)
                   (type-case Result (interp-env orelse env stry stk)
                      [v*s (velse selse) (v*s velse selse)]
                      [Return (velse selse) (Return velse selse)]
                      [Break (selse) (Break selse)]
                      [Continue (selse) (Continue selse)]
                      [Exception (velse selse) (Exception velse selse)])]
            [Return (vtry stry) (Return vtry stry)]
            [Break (stry) (Break stry)]
            [Continue (stry) (Continue stry)]
            ;; handle excepts here
            [Exception (vtry stry)
                       (local [(define excepts-r
                                 (interp-let exn-id (LocalId)
                                             vtry stry
                                             excepts
                                             env stk))]
                         (if (and (Exception? excepts-r)
                                  (is-obj-ptr? (Exception-v excepts-r)
                                               (Exception-s excepts-r))
                                  (object-is? (fetch-ptr (Exception-v excepts-r)
                                                         (Exception-s excepts-r))
                                              '$Reraise env (Exception-s excepts-r)))
                             (Exception vtry (Exception-s excepts-r))
                             excepts-r))])]

    [CTryFinally (try finally)
      (local [(define (replace-store [res : Result] [store : Store]) : Result
                (type-case Result res
                  [v*s (v s) (v*s v store)]
                  [Return (v s) (Return v store)]
                  [Break (s) (Break store)]
                  [Continue (s) (Continue store)]
                  [Exception (v s) (Exception v store)]))
              (define (get-store [expr : Result]) : Store
                (type-case Result expr
                  [v*s (v s) s]
                  [Return (v s) s]
                  [Break (s) s]
                  [Continue (s) s]
                  [Exception (v s) s]))
              (define try-r (interp-env try env sto stk))
              (define stry (get-store try-r))
              (define finally-r (interp-env finally env stry stk))
              (define rev-try (replace-store try-r (get-store finally-r)))
              (define to-return
                (type-case Result finally-r
                  [v*s (vfin sfin) rev-try]
                  [Return (vfin sfin) finally-r]
                  [Break (sfin) finally-r]
                  [Continue (sfin)
                            (mk-exception 'SyntaxError
                                          "'continue' not supported inside 'finally' clause"
                                          sfin)]
                  [Exception (vfin sfin)
                             (if (and (is-obj-ptr? vfin sfin)
                                      (object-is? (fetch-ptr vfin sfin) '$Reraise env sfin)
                                      (Exception? try-r))
                                 rev-try
                                 finally-r)]))]
        (if (v*s? try-r)
            to-return
            rev-try))]

    [CConstructModule (source)
       (handle-result (interp-env source env sto stk)
         (lambda (v-code s-code a)
           (cond
             [(not (and (VObjectClass? v-code)
                        (eq? (VObjectClass-antecedent v-code) 'code)))
              (error 'interp "a non-code object is passed to make module object")]
             [else
              (local [(define metacode (some-v (VObjectClass-mval v-code)))
                      (define global-var (MetaCode-globals metacode))
                      (define xcode (get-module-body (MetaCode-e metacode)))
                      
                      (define (inject-vars vars e s attr)
                        (cond [(empty? vars)
                               (values e s attr)]
                              [else
                               (let ((loc (new-loc))
                                     (sym (first vars)))
                                 (inject-vars (rest vars)
                                              (hash-set e sym loc)
                                              (hash-set s loc vnone)
                                              (hash-set attr sym loc)))]))
                      (define-values (new-env new-sto module-attr)
                        (inject-vars global-var
                                     (hash empty) ; NOTE: passing empty hash as env
                                     s-code
                                     (hash empty)))]
                 ; interpret the code in module, raise any exceptions as it is
                 ; ImportError should be handled in __import__
                 ; TODO: filter the built-in functions instead of interpreting python-lib again
                 (handle-result (interp-env (python-lib (CModule (CNone) xcode))
                                            (list new-env) new-sto stk)
                   (lambda (v-module s-module a)
                     (begin ;(pprint v-module)
                       (v*s (VObject '$module (none) module-attr) s-module)))))])))]
    
    [CBreak () (Break sto)]
    [CContinue () (Continue sto)])))

(define (assign-to-id [id : CExpr] [value : CVal] [env : Env] [sto : Store]) : Result
  (local [(define mayb-loc 
            (type-case IdType (CId-type id)
              [LocalId () (lookup (CId-x id) env)]
              [GlobalId () (lookup-global (CId-x id) env)]))]
(begin ;(display "mayb-loc:") (display  mayb-loc) (display "\n")
       ;(display "before assign, the store:")
       ;(if (some? mayb-loc) (pprint (fetch-once (some-v mayb-loc) sto)) (pprint "OH NO"))
       ;(display "after assign, the store:")
       ;(if (some? mayb-loc) (pprint value) (pprint "OLD STO"))
       ;(display "\n")
  (if (some? mayb-loc)
      (alloc-result vnone (hash-set sto (some-v mayb-loc) value))
      (type-case IdType (CId-type id)
                 [LocalId () (mk-exception 'NameError
                                           (string-append "name '"
                                                          (string-append (symbol->string (CId-x id))
                                                                         "' is not defined"))
                                           sto)]
                 [GlobalId () (mk-exception 'NameError
                                            (string-append "global name '"
                                                           (string-append (symbol->string (CId-x id))
                                                                          "' is not defined"))
                                            sto)])))))

(define (global-scope? [env : Env]) : boolean
  (= (length env) 1))

(define (print-class obj sto)
  (let* ([objv (fetch-ptr obj sto)])
    (type-case CVal objv
      [VObjectClass (antecedent mval dict cls)
       ;; TODO(joe): this shouldn't happen, typecheck to find out why
       (if (and (some? cls) (VUndefined? (some-v cls)))
         "VUndefined Class"
         (type-case (optionof CVal) cls
           [none () "No Class"]
           [some (v) (string-append (symbol->string antecedent)
                      (string-append ": "
                       (string-append (to-string (fetch-ptr v sto))
                        (string-append "@"
                         (to-string cls)))))]))]
      [else "Non-VObject"])))

;; handles lookup chain for function calls on objects
;; multiple inheritance modification : for class lookup call get-field-from-class
;; optional address field added to support self aliasing in bound methods calls.
(define (get-field [n : symbol] [cptr : CVal] [e : Env] [s : Store]) : Result
  (begin ;(display "GET: ") (display n) (display " ") (display cptr) (display "\n")
         ;(display (fetch-ptr cptr s)) (display "\n\n")
         ;(display (print-class cptr s)) (display "\n\n")
         ;(display " ") (display w_c) (display "\n\n")
         ;(display e) (display "\n\n")
  (cond
    [(not (is-obj-ptr? cptr s))
     (mk-exception 'AttributeError
                   (string-append 
                    (string-append (pretty cptr) " object has no attribute ")
                    (symbol->string n))
                   s)]
     ;; special attribute __class__
    [(eq? n '__class__)
     (v*s (get-class (fetch-ptr cptr s) e s) s)]
    [(is-special-method? n)
     ;; special methods are looked for in the class
     (get-field-from-obj n cptr (none) e s)]
    [(and (some? (VObjectClass-mval (fetch-ptr cptr s)))
          (MetaClass? (some-v (VObjectClass-mval (fetch-ptr cptr s)))))
     (begin ;(display (format "looking up in class: ~a\n" n))
     ;; class lookup
     (get-field-from-cls n cptr (none) e s))]
    [else
      ;; instance lookup
      (type-case CVal (fetch-ptr cptr s)
        [VObjectClass (antecedent mval d class) 
                 (let ([w (hash-ref (VObjectClass-dict (fetch-ptr cptr s)) n)])
                   (begin ;(display "loc: ") (display w) (display "\n\n")
                     (type-case (optionof Address) w
                       [some (w) 
                             (v*s (fetch-once w s) s)]
                       [none ()
     (begin ;(display (format "looking up in obj ~a\n" n))
      (let ([result (get-field-from-obj n cptr (none) e s)])
        (begin #;(display (format "Got: ~a\n" (fetch-ptr (v*s-v result) (v*s-s result))))
          result)))])))]
        [else (error 'interp "Not an object with fields.")])])))

(define (obj-ptr-match obj sto if-obj non-obj non-ptr)
  (type-case CVal obj
    [VPointer (a)
      (let ([v (fetch-once a sto)])
      (type-case CVal v
        [VObjectClass (ant m d c) (if-obj a ant m d c)]
        [else (non-obj v)]))]
    [else (non-ptr obj)]))

(define (assign-to-field o f [value : CVal] [env : Env] [sto : Store] [stk : Stack]) : Result
  (begin ;(display o) (display "---") (display f) (display "\n") (display value) (display "\n")
  (handle-result (interp-env o env sto stk)
    (lambda (vo so)
         (obj-ptr-match vo so
            (lambda (address antecedent mval d class)
             (local [(define loc (hash-ref d f))]
              (type-case (optionof Address) loc
                 [some (w) (alloc-result vnone (hash-set so w value))]
                 [none () (local [(define w (new-loc))
                                  (define snew
                                    (begin ;(display vo) (display "\n")
                                           ;(display objw) (display "\n")
                                    (hash-set so address 
                                              (VObjectClass antecedent
                                                       mval
                                                       (hash-set d f w)
                                                       class))))] ;; NOTE(joe) ensuring same class as above
                              (alloc-result vnone (hash-set snew w value)))])))
            (lambda (v) (error 'interp (format "Can't assign to nonobject ~a." v)))
            (lambda (vo) (error 'interp (format "Expected pointer, got ~a in assign-to-field" vo))))))))

;; bind-args, recursively binds the provided values to the provided argument symbols.
;; If a stararg symbol is provided, extra arguments are packaged in a tuple and bound
;; to the stararg symbol. This tuple will be empty if there are no extra arguments.
;; If no stararg symbol is provided, then an arity mismatch error is produced unless
;; the number of values corresponds to the number of argument symbols. Env is the 
;; environment in which we interpreted the arguments to get the values. This is needed
;; for creating exceptions and for a hacky check to see if we're creating a new object
;; (This should go away once mutability is actually builtin). Ext is the environment of
;; closure for which we are binding the values. That is, ext is the environment we're
;; extending.
(define (bind-args [args : (listof symbol)] 
                   [sarg : (optionof symbol)]
                   [vals : (listof Result)] 
                   [arges : (listof CExpr)] 
                   [env : Env] [ext : Env]
                   [sto : Store]) : (Env * Store * (optionof Result))
  (cond [(and (empty? args) (empty? vals))
         ;; This means we've bound all of the values to the argument symbols,
         ;; so we want to bind an empty tuple to the stararg symbol if there is one.
         (let ([vtuple (alloc-result (make-builtin-tuple empty) sto)])
           (if (some? sarg)
             (bind-args (list (some-v sarg))
                        (none)
                        (list vtuple)
                        (list (make-builtin-num 0))
                        env ext (v*s-s vtuple))
             (values ext (v*s-s vtuple) (none))))]
        ;need to bind star args!
        [(and (empty? args) (some? sarg)) 
         ;; This means we have a stararg symbol and extra arguments, so we'll
         ;; place them in a tuple and bind it to the stararg symbol.
         (let ([vtuple (alloc-result (make-builtin-tuple (map v*s-v vals)) sto)])
           (bind-args (list (some-v sarg))
                      (none) 
                      ;; NOTE(joe): this alloc-result feels dirty, but works
                      ;; because we need star-tuple to be a pointer to a tuple
                      (list vtuple)
                      (list (make-builtin-num 0))
                      env ext (v*s-s vtuple)))]
        [(or (empty? args) (empty? vals))
         ;; This means we have an arity mismatch.
         (values ext sto
                 (some
                   (mk-exception 'TypeError 
                                 (string-join
                                   (list "Arity mismatch: "
                                         "expected "
                                         (to-string args)
                                         ", received "
                                         (to-string (map v*s-v vals)))
                                   "")
                                 sto)))]
        [(and (cons? args) (cons? vals))
         ;; This means we're still binding values to argument symbols.
         ;; If the object is mutable, we want to use its current store location.
         ;; If the object is immutable, we can make a new store location.
         (local [(define val (first vals))
                 (define vv (v*s-v val))
                 (define loc (new-loc))
                 (define e (cons (hash-set (first ext) (first args) loc) (rest ext)))
                 ; TODO(Sumner): why env and not ext here?
                 (define s (hash-set sto loc vv))]
                (bind-args (rest args) sarg (rest vals) (rest arges) env e s))]))

(define (return-exception [sto : Store]) : Result
  (mk-exception 'SyntaxError "'return' outside function" sto))

(define (break-exception [sto : Store]) : Result
  (mk-exception 'SyntaxError "'break' outside loop" sto))

(define (continue-exception [sto : Store]) : Result
  (mk-exception 'SyntaxError "'continue' outside loop" sto))

(define (interp expr)
  (begin (reset-state)
  (type-case Result (interp-env expr (list (hash empty)) (hash empty) empty)
    [v*s (vexpr sexpr) (display "")]
    [Return (vexpr sexpr)
            (local [(define exn (return-exception sexpr))]
              (raise-user-error (string-append
                                  (pretty-exception (Exception-v exn)
                                                    (Exception-s exn)
                                                    #t)
                                  "")))]
    [Break (sexpr)
           (local [(define exn (break-exception sexpr))]
             (raise-user-error (string-append
                                 (pretty-exception (Exception-v exn)
                                                   (Exception-s exn)
                                                   #t)
                                 "")))]
    [Continue (sexpr)
           (local [(define exn (continue-exception sexpr))]
             (raise-user-error (string-append
                                 (pretty-exception (Exception-v exn)
                                                   (Exception-s exn)
                                                   #t)
                                 "")))] 
    [Exception (vexpr sexpr)
               (raise-user-error (string-append (pretty-exception vexpr sexpr #t) ""))])))

(define (truthy? [val : CVal] sto) : boolean
  (type-case CVal val
    [VClosure (e a s b o) true]
    [VObjectClass (a mval d class) (truthy-object? (VObjectClass a mval d class))]
    [VUndefined () false]
    [VPointer (a) (truthy? (fetch-once a sto) sto)]))

(define (interp-cprim2 [prim : symbol] 
                       [arg1 : CExpr]
                       [arg2 : CExpr]
                       [sto : Store]
                       [env : Env]
                       [stk : Stack]) : Result
    (handle-result (interp-env arg1 env sto stk)
      (lambda (varg1 sarg1)
           (handle-result (interp-env arg2 env sarg1 stk)
             (lambda (varg2 sarg2) 
                  (case prim
                    ;; Handle Is, IsNot, In, NotIn
                    ['Is (if (is? varg1 varg2 sarg2)
                          (v*s true-val sarg2)
                          (v*s false-val sarg2))]
                    ['IsNot (if (not (is? varg1 varg2 sarg2))
                           (v*s true-val sarg2)
                           (v*s false-val sarg2))]
                    [else (error 'interp (string-append "Haven't implemented a case yet: "
                                                        (symbol->string
                                                          prim)))]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multiple inheritance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mk-type will compute __mro__ for the class,
;; may return an exception if linearization is not possible
;; builtins/type.rkt would be a good place for this stuff,
;; it should handle type(name, bases, dict)
(define (mk-type [name : symbol]
                 [bases-ptr : CVal]
                 [h-dict : (hashof symbol Address)]
                 [sto : Store]
                 [env : Env]) : Result
  (begin ;(display "mk-type") (display name) (display "\n")
         ;(display bases-ptr) (display "\n") (display h-dict) (display "\n")
  (local [(define bases (fetch-ptr bases-ptr sto))
          (define bases-list (MetaTuple-v (some-v (VObjectClass-mval bases))))
          (define w (new-loc))
          (define bases_w (new-loc))
          (define mro_w (new-loc))]
    (handle-result (build-mro name bases-list env sto)
      (lambda (vmro smro) 
             (alloc-result (VObjectClass 'type
                           (some (MetaClass name)) 
                           (hash-set
                            (hash-set
                             (hash-set h-dict '__dict__ w)
                             '__bases__ bases_w)
                            '__mro__ mro_w)
                           (none))
                (let ([udict (make-under-dict h-dict env smro)])
                  (hash-set 
                    (hash-set 
                      (hash-set (v*s-s udict) w (v*s-v udict))
                      bases_w bases)
                    mro_w vmro))))))))

;; build-mro: merge the __mro__ of the bases using the C3 algorithm
;; Raises TypeError if there are duplicated bases or linearization is not possible.
;; The class should be the first element of __mro__, but since this seems hard
;; to implement with immutable hashes, it will be prepended on retrieval
(define (build-mro [name : symbol] 
                   [bases : (listof CVal)] 
                   [env : Env] 
                   [sto : Store]) : Result
  ;; The mro is the c3-merge of the mro of the bases plus the list of bases
  (let ([maybe-mro (c3-merge (append (map (lambda (base) (get-mro base (none) sto)) bases)
                                     (list bases)) empty)])
    (cond
      [(< (length (remove-duplicates bases)) (length bases))
       (mk-exception 'TypeError
                     (string-append 
                      "duplicate base class in class "
                      (symbol->string name))
                     sto)]
      [(none? maybe-mro) 
       (mk-exception 'TypeError
                     (string-append 
                      "cannot create a consisten method resolution order for class "
                      (symbol->string name))
                     sto)]
      [(some? maybe-mro)
       (begin 
         ;(display "class: ") (display name) (display " mro: ") 
         ;(display (map pretty (some-v maybe-mro))) (display "\n")
         ;(display "bases: ")
         ;(display (map pretty bases)) (display "\n")
         ;(display "stuff at bases: ")
         ;(display (map (lambda (b) (pretty (fetch-ptr b sto))) bases)) (display "\n")
         (alloc-result (VObjectClass 'tuple (some (MetaTuple (some-v maybe-mro))) (hash empty) (none))
              sto))])))
 
;; c3-merge: implements the c3 algorithm to merge mro lists
;; looks for a candidate (using c3-select)) and removes it from the mro lists
;; until all the mro lists are empty (success) or no candidate can be found (fail).
(define (c3-merge [xss : (listof (listof 'a))] 
                  [acc : (listof 'a)]) : (optionof (listof 'a))
  (let ([xss-ne (filter cons? xss)])
    (cond
      [(empty? xss-ne) (some acc)]
      [else (type-case (optionof 'b) (c3-select xss-ne 0)
              [none () (none)]
              [some (el) (c3-merge
                          (map (lambda (xs) 
                                 (filter (lambda (x) (not (eq? x el))) xs)) 
                               xss-ne)
                          (append acc (list el)))])])))

;; c3-select: looks sequentially for a head which doesn't appear in the tails
;; if none is found there is no c3 linearization possible
(define (c3-select [xss : (listof (listof 'a))] [n : number]) : (optionof 'a)
  (cond
    [(>= n (length xss)) (none)]
    [else (let ([el (first (list-ref xss n))])
            (if (any (map (lambda (xs) (member el (rest xs))) xss))
                (c3-select xss (add1 n))
                (some el)))]))

;; get-field-from-obj: looks for a field of an object using the class __mro__
;; skip up to thisclass in __mro__, if defined.
;; optional address field added to support self aliasing in bound methods calls.
(define (get-field-from-obj [fld : symbol] 
                            [objptr : CVal]
                            [thisclass : (optionof CVal)]
                            [env : Env] 
                            [sto : Store]) : Result
  (begin ;(display "GET-OBJ: ") (display fld) (display "\n"); (display " ") (display obj)
         ;(display " ") (display (fetch-ptr objptr sto)) (display "\n\n")
  (let ([obj (fetch-ptr objptr sto)])
    (cond
      ;; for method objects, __call__ attribute is the object itself
      [(and (equal? (VObjectClass-antecedent obj) 'method) (equal? fld '__call__))
       (v*s objptr sto)]
      ;; special lookup handling for initialized super object
      [(and (equal? (VObjectClass-antecedent obj) 'super)
            (some? (hash-ref (VObjectClass-dict obj) '__self__)))
       (local ([define w_self (hash-ref (VObjectClass-dict obj) '__self__)]
               [define self (fetch-once (some-v w_self) sto)]
               [define thisclass (fetch-once (some-v (hash-ref (VObjectClass-dict obj)
                                                                '__thisclass__))
                                              sto)])
         (cond
           [(and (is-obj-ptr? self sto)
                 (equal? (VObjectClass-antecedent (fetch-ptr self sto)) 'type))
            ;; obj.self is a class
            (get-field-from-cls fld self (some thisclass) env sto)]
           [else
            ;; obj.self is an instance
            (get-field-from-obj fld self (some thisclass) env sto)]))]
      ;; normal instance lookup
      [else
       (local ([define obj-cls (get-class obj env sto)])
         (type-case (optionof Address) (lookup-mro (get-mro obj-cls thisclass sto) fld sto)
           [some (w) (let ([value (fetch-once w sto)])
                      (begin
                        ;(display (format "Value from lookup-mro: ~a\n" (fetch-ptr value sto)))
                        ;(display (format "on field: ~a\n" fld))
                       (cond
                         ;; For functions, create method object bound to the object itself
                         [(VClosure? (fetch-ptr value sto)) 
                          (local [(define-values (meth sto-m) (mk-method w objptr (none) sto))]
                            (alloc-result meth sto-m))]
                         ;; for classmethod objects create method object bound to the object's class
                         [(and (is-obj-ptr? value sto) 
                               (equal? (VObjectClass-antecedent (fetch-ptr value sto)) 'classmethod))
                          (local [(define w_func 
                                    (some-v (hash-ref (VObjectClass-dict (fetch-ptr value sto)) '__func__)))
                                  (define-values (meth sto-m)
                                    ;; NOTE(joe): obj-cls needs to be a pointer
                                    (mk-method w_func obj-cls (none) sto))]
                            ;; NOTE(joe): does this alloc-result break is-equality of class methods?
                            (alloc-result meth sto-m))]
                         ;; for staticmethod obj. return func attribute
                         [(and (is-obj-ptr? value sto) 
                               (equal? (VObjectClass-antecedent (fetch-ptr value sto)) 'staticmethod))
                                    (get-field '__func__ value env sto)]
                         ;; otherwise return the value of the attribute
                         [else 
                          (v*s value sto)])))]
           [none () (mk-exception 'AttributeError
                                  (string-append 
                                   (string-append "object " 
                                                  (symbol->string (VObjectClass-antecedent obj)))
                                   (string-append " has no attribute "
                                                  (symbol->string fld)))
                                  sto)]))]))))

;; get-field-from-cls: looks for a field of a class using class __mro__
;; skip up to thisclass in __mro__, if defined.
;; optional address field added to support self aliasing in bound methods calls.
(define (get-field-from-cls [fld : symbol] 
                            [clsptr : CVal]
                            [thisclass : (optionof CVal)]
                            [env : Env] 
                            [sto : Store]) : Result
  (begin ;(display "GET-CLS: ") (display fld) ;(display " ") (display clsptr)
         ;(display " ") (display clsptr) (display "\n")
  (let ([cls (fetch-ptr clsptr sto)])
    (cond
      [(equal? fld '__mro__) 
       ;; temporary hack to avoid self-reference in __mro__
       (alloc-result (VObjectClass 'tuple (some (MetaTuple (get-mro clsptr thisclass sto))) (hash empty) (none))
            sto)]
      [else
       (type-case (optionof Address) (lookup-mro (get-mro clsptr thisclass sto) fld sto)
         [some (w)
               (let ([value (fetch-once w sto)])
                 (cond
                   ;; for classmethod obj. create method obj. bound to the class
                   [(and (is-obj-ptr? value sto) 
                         (equal? (VObjectClass-antecedent (fetch-ptr value sto)) 'classmethod))
                    (local [(define w_func (some-v (hash-ref (VObjectClass-dict (fetch-ptr value sto)) '__func__)))
                            (define-values (meth sto-m) (mk-method w_func clsptr (none) sto))]
                      (alloc-result meth sto-m))]
                   ;; for staticmethod obj. return func attribute
                   [(and (is-obj-ptr? value sto)
                         (equal? (VObjectClass-antecedent (fetch-ptr value sto)) 'staticmethod))
                    (get-field '__func__ value env sto)]
                   ;; otherwise return the value of the attribute
                   [else
                    (begin ;(display "Else of get-cls\n") (display value)
                    ;(display "\n") (display (fetch-ptr value sto)) (display "\n")
                    (v*s value sto))]))]
         [none () (mk-exception 'AttributeError
                                (string-append 
                                 (string-append "class " 
                                                (symbol->string (VObjectClass-antecedent cls)))
                                 (string-append " has no attribute "
                                                (symbol->string fld)))
                                sto)])]))))

;; lookup-mro: looks for field in mro list
(define (lookup-mro [mro : (listof CVal)] [n : symbol] [sto : Store]) : (optionof Address)
  (cond
    [(empty? mro) (none)]
    [(cons? mro)
     (cond
      [(is-obj-ptr? (first mro) sto)
       (type-case CVal (fetch-ptr (first mro) sto)
            [VObjectClass (antecedent mval d class)
                     (type-case (optionof Address) (hash-ref d n)
                       [none () (lookup-mro (rest mro) n sto)]
                       [some (value) (some value)])]
            [else (error 'lookup-mro "an entry in __mro__ list is not an object")])])]))

;; special methods
(define (is-special-method? [n : symbol])
  (member n (list '__call__ '__eq__ '__cmp__ '__str__ '__getitem__ '__gt__ '__lt__ '__lte__ '__gte__)))
