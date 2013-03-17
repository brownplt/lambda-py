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

(define (handle-result env result fun)
   (type-case Result result
     [v*s (v s) (fun v s)]
     [Return (v s) (return-exception env env s)]
     [Break (s) (break-exception env env s)]
     [Continue (s) (continue-exception env env s)] 
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
                     [Break (sb) (break-exception env sb)]
                     [Continue (sb) (continue-exception env sb)]
                     [Exception (vb sb) (Exception vb sb)]))])]
    [else (error 'interp (string-append "Not a closure: " (to-string vfun)))]))

(define (interp-capp [fun : CExpr] [arges : (listof CExpr)] 
                     [stararg : (optionof CExpr)]
                     [env : Env] [sto : Store] [stk : Stack]) : Result
  (begin ;(display "APP: ") (display fun) (display "\n") (display arges) (display "\n\n\n")
         ;(display env) (display "\n\n")
 (handle-result env (interp-env fun env sto stk)
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
                      [Return (vfun sfun) (return-exception env sfun)]
                      [Break (sfun) (break-exception env sfun)]
                      [Continue (sfun) (continue-exception env sfun)]
                      [Exception (vfun sfun) (mk-exception 'TypeError
                                                           (string-append 
                                                             (symbol->string b)
                                                             " object is not callable")
                                                           env
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
                                                      env
                                                      sto)]
                         [else (v*s full-val sto)])))
                     (local [(define full-w (lookup id env))]
                       (if (some? full-w)
                           (local [(define full-val (fetch-once (some-v full-w) sto))]
                             (type-case CVal full-val
                               [VUndefined () (mk-exception 'NameError freevar-error-str env sto)]
                               [else (v*s full-val sto)]))
                           (mk-exception 'NameError
                                         (string-append "global " name-error-str)
                                         env
                                         sto))))))]
      [GlobalId ()
                (local [(define full-w (lookup-global id env))]
                 (begin ;(display "Global ") (display id) (display " = ") (display full-w)
                  (if (some? full-w)
                      (local [(define full-val (fetch-once (some-v full-w) sto))]
                        (type-case CVal full-val
                          [VUndefined () (mk-exception 'NameError name-error-str env sto)]
                          [else (v*s full-val sto)]))
                      (mk-exception 'NameError name-error-str env sto))))])))

;; interp-env : CExpr * Env * Store * Stack -> Result
(define (interp-env [expr : CExpr] [env : Env] [sto : Store] [stk : Stack]) : Result
  (begin ;(display expr) (display "\n")
         ;(display env) (display "\n\n")
  (type-case CExpr expr
    [CModule (prelude body)
             (local [(define prelude-r (interp-env prelude env sto stk))]
                (handle-result env prelude-r
                  (lambda (v s) (interp-env body env s stk))))]
    
    [CSym (s) (v*s (VSym s) sto)]
    [CTrue () (renew-true env sto)]
    [CFalse () (renew-false env sto)]
    [CNone () (alloc-result vnone sto)]
    [CUndefined () (v*s (VUndefined) sto)]

    [CClass (name)
            (alloc-result (VObjectClass 'type
                                        (some (MetaClass name))
                                        (hash empty)
                                        (none))
                          sto)]

    [CGetField (value attr)
    (begin
      ;(display "Getting field ") (display attr) (display "from: \n") (display value)
      ;(display "\n\n")
               (handle-result env (interp-env value env sto stk)
                          (lambda (vval sval) (get-field attr vval env sval))))]
			
    [CSeq (e1 e2) (type-case Result (interp-env e1 env sto stk)
                    [v*s (v1 s1) (interp-env e2 env s1 stk)]
                    [Return (v1 s1) (Return v1 s1)]
                    [Break (s1) (Break s1)]
                    [Continue (s1) (Continue s1)] 
                    [Exception (v1 s1) (Exception v1 s1)])]
    
    [CSet (class values)
     (type-case ResultList (interp-cascade values sto env stk)
      [Abnormal (r) r]
      [v*s/list (result-list new-s)
       (handle-result env (interp-env class env new-s stk)
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
       (handle-result env (interp-env class env new-s stk)
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
       (handle-result env (interp-env class env new-s stk)
        (lambda (cval csto)
         (let ([val-list (map v*s-v result-list)])
          (alloc-result (VObjectClass 'tuple
                       (some (MetaTuple val-list))
                       (hash empty)
                       (some cval))
              csto))))])]

    [CAssign (t v) 
             (begin ;(display "\nASSIGN: ") (display t) (display " | ") (display v) (display "\n")
               (handle-result env (interp-env v env sto stk)
                 (lambda (vv sv)
                      (type-case CExpr t
                        [CId (x type) (assign-to-id t vv env sv)]
                        [CGetField (o a) (assign-to-field o a vv env sv stk)]
                        [else (mk-exception 'SyntaxError
                                            "can't assign to literals"
                                            env
                                            sv)]))))]
    
    [CIf (i t e) (handle-result env (interp-env i env sto stk)
                   (lambda (vi si) (if (truthy? vi si)
                                       (interp-env t env si stk)
                                       (interp-env e env si stk))))]
    
    [CId (x t) (interp-id x t env sto)]

    [CObject (class mval)
             (handle-result env (interp-env class env sto stk)
               (lambda (cval csto)
                 (alloc-result (VObjectClass (MetaClass-c (some-v (VObjectClass-mval (fetch-ptr cval csto))))
                                    mval
                                    (hash empty)
                                    (some cval))
                      csto)))]

    [CLet (x type bind body)
          (begin ;(display "LET: ") (display x) (display " ")
                 ;(display type) (display bind) (display "\n")
          (handle-result env (interp-env bind env sto stk)
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
             (handle-result env (interp-env value env sto stk)
               (lambda (vv sv) (Return vv sv)))]

    [CWhile (body test orelse) (interp-while body test orelse env sto stk)]

    [CBuiltinPrim (op args) 
                  (type-case ResultList (interp-cascade args sto env stk)
                   [Abnormal (r) r]
                   [v*s/list (result-list new-s)
                    (local [(define val-list (map v*s-v result-list))]
                            (builtin-prim op val-list env new-s stk))])]
    [CRaise (expr) 
            (if (some? expr)
                (handle-result env (interp-env (some-v expr) env sto stk)
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
                                             env
                                             sexpr)]))))
                (mk-exception '$Reraise
                              "reraise previous exception if possible"
                              env
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
                                          env
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
       (handle-result env (interp-env source env sto stk)
         (lambda (v-code s-code)
           (cond
             [(not (and (is-obj-ptr? v-code s-code)
                        (eq? (VObjectClass-antecedent (fetch-ptr v-code s-code)) 'code)))
              (error 'interp (format "a non-code object ~a is passed to make module object" v-code))]
             [else
              (let ((obj (fetch-ptr v-code s-code)))
                (local [(define metacode (some-v (VObjectClass-mval obj)))
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
                                        ; interpret the code in module, raise any exception as it is
                                        ; ImportError should be handled in __import__
                                        ; TODO: filter the built-in functions instead of interpreting python-lib again
                       (handle-result env (interp-env (python-lib (CModule (CNone) xcode))
                                                      (list new-env) new-sto stk)
                                      (lambda (v-module s-module)
                                        (begin ;(pprint v-module)
                                          (alloc-result (VObject '$module (none) module-attr)
                                                        s-module))))))])))]
    
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
                                           env
                                           sto)]
                 [GlobalId () (mk-exception 'NameError
                                            (string-append "global name '"
                                                           (string-append (symbol->string (CId-x id))
                                                                          "' is not defined"))
                                            env
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
                   e
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
  (handle-result env (interp-env o env sto stk)
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
                                 env
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

(define (return-exception env [sto : Store]) : Result
  (mk-exception 'SyntaxError "'return' outside function" env sto))

(define (break-exception env [sto : Store]) : Result
  (mk-exception 'SyntaxError "'break' outside loop" env sto))

(define (continue-exception env [sto : Store]) : Result
  (mk-exception 'SyntaxError "'continue' outside loop" env sto))

(define (interp expr)
  (begin (reset-state)
  (type-case Result (interp-env expr (list (hash empty)) (hash empty) empty)
    [v*s (vexpr sexpr) (display "")]
    [Return (vexpr sexpr)
     (raise-user-error (format "Unexpected return reached toplevel: ~a" vexpr))]
    [Break (sexpr)
     (raise-user-error (format "Unexpected break reached toplevel"))]
    [Continue (sexpr)
     (raise-user-error (format "Unexpected continue reached toplevel"))]
    [Exception (vexpr sexpr)
               (raise-user-error (string-append (pretty-exception vexpr sexpr #t) ""))])))

(define (truthy? [val : CVal] sto) : boolean
  (type-case CVal val
    [VClosure (e a s b o) true]
    [VObjectClass (a mval d class) (truthy-object? (VObjectClass a mval d class))]
    [VUndefined () false]
    [VSym (t) (equal? t 'true)]
    [VPointer (a) (truthy? (fetch-once a sto) sto)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multiple inheritance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                                  env
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
                                env
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
