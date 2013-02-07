#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "builtins/object.rkt"
         "builtins/bool.rkt"
         "builtins/tuple.rkt"
         "builtins/num.rkt"
         "builtins/none.rkt"
         "builtins/dict.rkt"
         "builtins/method.rkt"
         "util.rkt"
<<<<<<< HEAD
=======
         (typed-in racket/base (hash-copy : ((hashof 'a 'b) -> (hashof 'a 'b))))
         (typed-in racket/base (hash-map : ((hashof 'a 'b) ('a 'b -> 'c) -> (listof 'c))))
>>>>>>> master
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
         )

(define (append3 a b c)
  (append a (append b c)))

;; interp-cascade, interprets a list of expressions with an initial store,
<<<<<<< HEAD
;; environment and produces the list of results and the final store
(define (interp-cascade [exprs : (listof CExpr)] 
                        [init-s : Store]
                        [env : Env]) : ((listof Result) * Store)
=======
;; environment and produces the list of results and the final environment and
;; store using the values/define-values and stack of activation records
(define (interp-cascade [exprs : (listof CExpr)] 
                        [init-s : Store]
                        [init-e : Env]
                        [stk : Stack]) : ((listof Result) * Store * Env)
>>>>>>> master
  (local [(define (rec-cascade exprs e s)
            (cond [(empty? exprs) empty]
                  [(cons? exprs)
                     (let ([first-r (interp-env (first exprs) e s stk)])
                       (type-case Result first-r
<<<<<<< HEAD
                         [v*s (vfr sfr afr) (cons first-r (rec-cascade (rest exprs) e sfr))]
                         [Return (vfr sfr afr) (list (return-exception sfr))]
                         [Break (sfr) (list first-r)]
                         [Continue (sfr) (list first-r)] 
                         [Exception (vfr sfr) (list first-r)]))]))
          (define result-list (rec-cascade exprs env init-s))
          (define non-results
            (filter (lambda (r)
                      (or (Exception? r) (Break? r)  (Continue? r)))
                    result-list))]
      (if (< 0 (length non-results))
          (values non-results 
                  (cond
                    [(Exception? (first non-results)) (Exception-s (first non-results))]
                    [(Break? (first non-results)) (Break-s (first non-results))]
                    [(Continue? (first non-results)) (Continue-s (first non-results))]))
          (values result-list
                  (if (cons? result-list)
                      (v*s-s (first (reverse result-list)))
                      init-s)))))

;; interp-capp, interprets an application of a function. It is passed the function,
;; the list of argument expressions, an optionof indicating the presence of a stararg,
;; and the env, and the store. Returns the result of interpreting the application.
(define (interp-capp [fun : CExpr] [arges : (listof CExpr)] 
                     [stararg : (optionof CExpr)] [env : Env] [sto : Store]) : Result
 (type-case Result (interp-env fun env sto)
   [v*s (vfun sfun afun) 
    (type-case CVal vfun
      ;; fun is a closure, bind args and apply it
      [VClosure (cenv argxs vararg body)
                (local [(define-values (argvs sc) (interp-cascade arges sfun env))
                        (define exn? (filter Exception? argvs))]
                  (if (< 0 (length exn?))
                      (first exn?)
                      (local [(define result
                                (if (some? stararg)
                                    (local [(define sarg-r
                                              (interp-env (some-v stararg) env sc))
                                            ;; todo: support other types
                                            ;; for star args
                                            (define l (MetaTuple-v 
                                                        (some-v 
                                                          (VObjectClass-mval 
                                                            (v*s-v sarg-r)))))
                                            (define lr (map (lambda (x)
                                                              (v*s x (v*s-s sarg-r) (none)))
                                                            l))
                                            (define le (map (lambda (x)
                                                              (make-builtin-num 0))
                                                            l))]
                                      (bind-and-execute body argxs vararg 
                                                        (append argvs lr)
                                                        (append arges le)
                                                        env cenv
                                                        (v*s-s sarg-r))) 
                                    (bind-and-execute body argxs vararg
                                                      argvs arges env
                                                      cenv sc)))]
                        (type-case Result result
                          [v*s (vb sb ab) (v*s vnone sb (none))]
                          [Return (vb sb ab) (v*s vb sb ab)]
                          [Break (sb) (break-exception sb)]
                          [Continue (sb) (continue-exception sb)] 
                          [Exception (vb sb) (Exception vb sb)]))))]
     [VObjectClass (b mval d class)
               (if (and (some? mval) (MetaClass? (some-v mval)))
                  ;; This means we're calling a class, like C().
                  ;; So we want to apply its __init__ method.
                  (local [(define f (v*s-v (get-field '__init__ vfun env sfun)))
                          ;; Create an empty object. This will be the new instance of the class.
                          (define o (new-object (MetaClass-c (some-v mval)) afun))]
                    (type-case CVal f
                      [VClosure (cenv argxs vararg body)
                                ;; Interpret the arguments to __init__.
                         (local [(define-values (argvs sc) (interp-cascade arges sfun env))
                                 (define exn? (filter Exception? argvs))]
                           (if (< 0 (length exn?))
                               (first exn?)
                               (local [(define loc (new-loc))
                                       (define nid (new-id))
                                       ;; Bind the interpreted arguments to __init__
                                       ;; and perform the application.
                                       (define result 
                                         (bind-and-execute body argxs vararg
                                                           (cons (v*s o sfun (some loc)) argvs)
                                                           (cons (CId nid (LocalId)) arges)
                                                           (cons (hash-set (first env) nid loc)
                                                                 (rest env))
                                                           cenv
                                                           (hash-set sc loc o)))]
                                 (type-case Result result
                                   [v*s (vb sb ab) 
                                        ;; This is a bit hacky. We just created an object
                                        ;; and passed it to __init__ which may have mutated
                                        ;; it, so we need to get the original object from
                                        ;; it's first location in the store.
                                        ;; TODO is this needed anymore? I don't think so.
                                        (begin ;(display o) (display "\n")
                                               ;(display env) (display "\n")
                                               ;(display sb) (display "\n")
                                        (v*s (fetch loc sb) sb (none)))]
                                   [Return (vb sb ab)
                                           (if (and (VObjectClass? vb)
                                                    (some? (VObjectClass-mval vb))
                                                    (MetaNone? (some-v (VObjectClass-mval vb))))
                                               (v*s (fetch loc sb) sb (none))
                                               (mk-exception 'TypeError
                                                             "__init__() should return None"
                                                             sb))]
                                   [Break (sb) (break-exception sb)]
                                   [Continue (sb) (continue-exception sb)] 
                                   [Exception (vb sb) (Exception vb sb)]))))]
                     [else (error 'interp 
                                   "__init__ not found. THIS SHOULDN'T HAPPEN.")]))
                  ;; This means we're calling an object,
                  ;; so we want to apply its __call__ method.
                  (local [(define __call__ (get-field '__call__ vfun env sfun))]
                    (type-case Result __call__
                      [v*s (vc sc ac)
                           (type-case CVal vc
                             [VClosure (cenv argxs vararg body)
                                       ;; Interpret the arguments to __call__.
                               (local [(define-values (argvs sc)
                                           (interp-cascade arges sfun env))
                                       (define exn? (filter Exception? argvs))]
                                 (if (< 0 (length exn?))
                                     (first exn?)
                                     (local [;; Bind the interpreted arguments to __call__
                                             ;; and perform the application.
                                             (define result 
                                               (bind-and-execute body argxs vararg
                                                                 (cons (v*s vfun sfun afun) argvs)
                                                                 (cons (make-builtin-num 0) arges)
                                                                 env
                                                                 cenv
                                                                 sc))]
                                       (type-case Result result
                                         [v*s (vb sb ab) (v*s vb sb ab)]
                                         [Return (vb sb ab) (v*s vb sb ab)]
                                         [Break (sb) (break-exception sb)]
                                         [Continue (sb) (continue-exception sb)] 
                                         [Exception (vb sb) (Exception vb sb)]))))]
                             [else (error 'interp "Not a closure or constructor.")])]
                      [Return (vfun sfun afun) (return-exception sfun)]
                      [Break (sfun) (break-exception sfun)]
                      [Continue (sfun) (continue-exception sfun)] 
                      [Exception (vfun sfun) (Exception vfun sfun)])))]
     [else (error 'interp "Not a closure or constructor")])]
   [Return (vfun sfun afun) (return-exception sfun)]
   [Break (sfun) (break-exception sfun)]
   [Continue (sfun) (continue-exception sfun)] 
   [Exception (vfun sfun) (Exception vfun sfun)]))

(define (interp-while [test : CExpr] [body : CExpr]
                      [orelse : CExpr] [env : Env]
                      [sto : Store]) : Result
  (local [(define test-r (interp-env test env sto))]
    ;; if the test results in an exception, pass it along
    (if (Exception? test-r)
        test-r
        (if (truthy? (v*s-v test-r))
            (local [(define body-r (interp-env body env (v*s-s test-r)))]
              (cond
                ;; if the body results in an exception or return, pass it along
                [(or (Exception? body-r) (Return? body-r)) body-r]
                ;; if it results in a break, return None
                [(Break? body-r) (v*s vnone (Break-s body-r) (none))]
                ;; if it resulted in a value or continue, attempt to run the loop again
                [else (interp-while test body orelse env (v*s-s body-r))]))
            (interp-env orelse env (v*s-s test-r))))))

;; bind-and-execute, binds the arguments in the closure's
;; environment and then applies the closure.
(define (bind-and-execute [body : CExpr] [argxs : (listof symbol)]
                          [vararg : (optionof symbol)] [argvs : (listof Result)]
=======
                         [v*s*e (vfr sfr efr)
                                (cons first-r (rec-cascade (rest exprs) efr sfr))]
                         [Return (vfr sfr efr) (list (return-exception efr sfr stk))]
                         [Break (sfr efr) (list first-r)]
                         [Exception (vfr sfr efr) (list first-r)]))]))
          (define result-list (rec-cascade exprs init-e init-s))]

         (let ([ex-results (filter Exception? result-list)]
               [break-results (filter Break? result-list)])
           (if (< 0 (length ex-results))
               (values ex-results
                       (Exception-s (first ex-results))
                       (Exception-e (first ex-results)))
               (if (< 0 (length break-results))
                   (values break-results
                           (Break-s (first break-results))
                           (Break-e (first break-results)))
                   (values result-list
                           (if (cons? result-list)
                               (v*s*e-s (first (reverse result-list)))
                               init-s)
                           (if (cons? result-list)
                               (v*s*e-e (first (reverse result-list)))
                               init-e)))))))

;; common code to interpret function and method application, first argument must be a VClosure.
(define (interp-vclosure [vfun : CVal] [arges : (listof CExpr)] 
                         [stararg : (optionof CExpr)]
                         [efun : Env] [sfun : Store] [stk : Stack] [env : Env]) : Result
  (type-case CVal vfun
    [VClosure (cenv argxs vararg body opt-class)
              (local [(define-values (argvs-r sc ec) (interp-cascade arges sfun efun stk))]
                (let ([exn? (filter Exception? argvs-r)])
                  (if (< 0 (length exn?))
                      (first exn?)
                      (local [(define argvs (map v*s*e-v argvs-r))
                              (define result (if (some? stararg)
                                                 (letrec ([sarg-r (interp-env (some-v
                                                                               stararg) ec sc stk)]
                                                          ;; todo: support other types
                                                          ;; for star args
                                                          [l (MetaTuple-v 
                                                              (some-v 
                                                               (VObject-mval 
                                                                (v*s*e-v
                                                                 sarg-r))))])
                                                   (bind-and-execute 
                                                    body opt-class argxs vararg 
                                                    (append argvs l)
                                                    (append arges (map
                                                                   (lambda(x)
                                                                     (make-builtin-num 0))
                                                                   l))
                                                    (v*s*e-e sarg-r)
                                                    cenv
                                                    (v*s*e-s sarg-r)
                                                    stk env)) 
                                                 (bind-and-execute body opt-class argxs vararg
                                                                   argvs arges ec
                                                                   cenv
                                                                   sc
                                                                   stk env)))]
                        (type-case Result result
                          [v*s*e (vb sb eb) (v*s*e vnone sb env)]
                          [Return (vb sb eb) (v*s*e vb sb env)]
                          [Break (sb eb) (break-exception env sb stk)]
                          [Exception (vb sb eb) (Exception vb sb env)])))))]
    [else (error 'interp "Not a closure or constructor.")]))

(define (interp-capp [fun : CExpr] [arges : (listof CExpr)] 
                     [stararg : (optionof CExpr)]
                     [env : Env] [sto : Store] [stk : Stack]) : Result
  (begin ;(display "APP: ") (display fun) (display "\n") (display arges) (display "\n\n\n")
         ;(display env) (display "\n\n")
 (type-case Result (interp-env fun env sto stk)
   [v*s*e (vfun sfun efun) 
    (type-case CVal vfun
      [VClosure (cenv argxs vararg body opt-class)
                (interp-vclosure vfun arges stararg efun sfun stk env)]

      [VObject (b mval d)
               (if (and (some? mval) (MetaClass? (some-v mval)))
                  ; We're calling a class.
                  ; Get its constructor
                  (let ([f (v*s*e-v (get-field '__init__ vfun (none) efun sfun stk))]
                        ; Create an empty object. This will be the instance of that class.
                        [o (new-object (MetaClass-c (some-v mval)) efun sfun)])
                    (type-case CVal f
                      [VClosure (cenv argxs vararg body opt-class)
                                ; interpret the arguments to the constructor
                         (local [(define-values (argvs-r sc ec)
                                   (interp-cascade arges sfun efun stk))]
                                (let ([exn? (filter Exception? argvs-r)])
                                    (if (< 0 (length exn?))
                                        (first exn?)
                                      (local [(define argvs (map v*s*e-v argvs-r))
                                             ; bind the interpreted arguments to the constructor
                                              (define result 
                                                (bind-and-execute body opt-class argxs vararg
                                                                (cons o argvs)
                                                                (cons (CId 'init (LocalId)) 
                                                                      arges)
                                                                ec
                                                                cenv
                                                                sc
                                                                stk env))]
                                        (type-case Result result
                                          [v*s*e (vb sb eb) 
                                                 (v*s*e 
                                                   (let ([obj (fetch (some-v 
                                                                       (lookup (first argxs)
                                                                               eb))
                                                                     sb)])
                                                     obj)
                                                   sb env)]
                                         [Return (vb sb eb)
                                                 (v*s*e vb sb env)]
                                         [Break (sb eb) (break-exception env sb stk)]
                                         [Exception (vb sb eb)
                                                 (Exception vb sb env)])))))]
                      [else (error 'interp 
                                   "__init__ not found. THIS SHOULDN'T HAPPEN.")]))
                                     
                  (local [(define __call__ (get-field '__call__ vfun (none) efun sfun stk))]
                    (type-case Result __call__
                      [v*s*e (vc sc ec)
                             (cond
                               ;; for bound methods use __func__ attribute and __self__
                               [(and (VObject? vc) (equal? (VObject-antecedent vc) 'method))
                                (local 
                                  [(define func
                                     (fetch (some-v (hash-ref (VObject-dict vc) '__func__)) sc))
                                   (define w_self 
                                     (hash-ref (VObject-dict vc) '__self__))
                                   (define id_self (new-id))
                                   (define m_arges
                                     (cons (CId id_self (LocalId)) arges))
                                   ;; extend the environment with self to support self aliasing
                                   (define m_env
                                     ;; The environment is not allways immutable, it shouldn't
                                     ;; happen with the new scope...
                                     (cons (hash-set (if (immutable? (first efun))
                                                         (first efun)
                                                         (hash (hash-map (first efun) 
                                                                         (lambda (k v) (values k v)))))
                                                     id_self (some-v w_self)) 
                                           (rest efun)))]
                                  (interp-vclosure func m_arges stararg m_env sc stk env))]
                               [else
                                ;; for unbound methods, use function application
                                (interp-vclosure vc arges stararg efun sfun stk env)])]
                      [Return (vfun sfun efun) (return-exception efun sfun stk)]
                      [Break (sfun efun) (break-exception efun sfun stk)]
                      [Exception (vfun sfun efun) (mk-exception 'TypeError
                                                                (string-append 
                                                                 (symbol->string b)
                                                                 " object is not callable")
                                                                env sto stk)])))]
      [else (error 'interp "Not a closure or constructor")])]
   [Return (vfun sfun efun) (return-exception efun sfun stk)]
   [Break (sfun efun) (break-exception efun sfun stk)]
   [Exception (vfun sfun efun) (Exception vfun sfun efun)])))

(define (interp-while [test : CExpr] [body : CExpr] 
                      [env : Env] [sto : Store] [stk : Stack])
  (local [(define test-r (interp-env test env sto stk))]
    (if (or (or (Exception? test-r) (Return? test-r)) (Break? test-r))
      test-r
      (if (truthy? (v*s*e-v test-r))
        (local [(define body-r (interp-env body (v*s*e-e test-r)
                                                (v*s*e-s test-r) stk))]
               (if (or (Exception? body-r) (Return? body-r))
                 body-r
                 (if (Break? body-r)
                   (v*s*e
                     vnone
                     (Break-s body-r)
                     (Break-e body-r)) 
                   (interp-while test body 
                                 (v*s*e-e body-r) 
                                 (v*s*e-s body-r) stk))))
        (v*s*e
          vnone
          (v*s*e-s test-r)
          (v*s*e-e test-r))))))



(define (bind-and-execute [body : CExpr]
                          [opt-class : (optionof symbol)]
                          [argxs : (listof symbol)]
                          [vararg : (optionof symbol)] [argvs : (listof CVal)]
>>>>>>> master
                          [arges : (listof CExpr)] [env : Env]
                          [ext : Env] [sto : Store] [stk : Stack] [dyn : Env]) : Result
  (local [(define-values (e s mayb-ex) 
            (bind-args argxs vararg argvs arges env ext sto stk))]
    (if (some? mayb-ex)
        (some-v mayb-ex)
        (local [(define class 
                  (if (some? opt-class)
                      ;; fetch class using the closure's environment
                      (some (fetch (some-v (lookup (some-v opt-class) ext)) sto))
                      (none)))
                (define self 
                  (if (and (some? opt-class) (> (length argvs) 0))
                      ;; self is the first argument, if any, for methods
                      (some (first argvs))
                      (none)))]
        (interp-env body e s 
                    ;; push new activation record on the stack
                    ;; used the dynamic environment for compatibility with base code.
                    (cons (Frame dyn class self) stk))))))

(define (interp-excepts [excepts : (listof CExpr)]
                        [sto : Store]
                        [env : Env]
<<<<<<< HEAD
                        [exn : Result]) : Result
  (local [(define exn-type (VObjectClass-antecedent (Exception-v exn)))
          (define (find-match type exps fsto)
=======
                        [exn : Result]
                        [stk : Stack]) : Result
  (local [(define exn-type (VObject-antecedent (Exception-v exn)))
          (define (find-match type exps fsto fenv)
>>>>>>> master
            (cond
              [(empty? exps) (values (none) fsto (none))]
              [(cons? exps)
               ;; need to interp exprs and then check
<<<<<<< HEAD
               (local [(define-values (except-types-results tsto)
                         (interp-cascade (CExcept-types (first exps)) fsto env))
=======
               (local [(define-values (except-types-results tsto tenv)
                         (interp-cascade (CExcept-types (first exps)) fsto fenv stk))
>>>>>>> master
                       (define exn? (filter Exception? except-types-results))]
                 (if (< 0 (length exn?))
                     (values (none) tsto (some (first exn?)))
                     (local [(define actual-except-types
                               (if (and (> (length except-types-results) 0)
                                        (VObjectClass? (v*s-v (first except-types-results)))
                                        (MetaTuple?
                                          (some-v (VObjectClass-mval
                                                    (v*s-v (first except-types-results))))))
                                   (map (λ (v) (v*s v
                                                    (v*s-s (first except-types-results))
                                                    (none)))
                                        (MetaTuple-v
                                          (some-v (VObjectClass-mval
                                                    (v*s-v (first except-types-results))))))
                                   except-types-results))
                             (define except-types
                                 (begin ;(display (map v*s-v actual-except-types)) (display "\n")
                                 (map (lambda (t)
                                         (type-case CVal (v*s-v t)
                                           [VObjectClass (ante mval dict class) 
                                                    (if (and (some? mval) 
                                                             (MetaClass? (some-v mval)))
                                                      (some (MetaClass-c (some-v mval)))
                                                      (none))]
                                           [else (none)]))
                                       actual-except-types)))
                             (define exn-again? (filter none? except-types))]
                                (if (< 0 (length exn-again?))
                                  (values (none) tsto
                                          (some (mk-exception
                                                  'TypeError
                                                  "can't catch closures. This will go away."
<<<<<<< HEAD
                                                  tsto)))
=======
                                                  tenv
                                                  tsto
                                                  stk)))
>>>>>>> master
                                  (if (or (member exn-type (map some-v except-types))
                                          (empty? (CExcept-types (first exps))))
                                    (values (some (first exps)) tsto (none))
                                    (find-match type (rest exps) tsto))))))]))
          (define-values (match? hsto exn?) (find-match exn-type excepts sto))]

    ; we might have found a matching except clause
    (if (some? exn?)
      (some-v exn?)
      (if (some? match?)
        (let ([as-name (CExcept-name (some-v match?))])
          (let ([result
                  (if (some? as-name)
                    (interp-let (some-v as-name)
<<<<<<< HEAD
                                ;; This could potentially be in global scope. Does it matter?
                                (LocalId)
                                (Exception (Exception-v exn) hsto)
                                (CExcept-body (some-v match?))
                                env)
                    (interp-env (some-v match?) env hsto))])
            (type-case Result result
              [v*s (vbody sbody abody) (v*s vnone sbody (none))]
              [Return (vbody sbody abody) (return-exception sbody)]
              [Break (sbody) (Break sbody)]
              [Continue (sbody) (Continue sbody)] 
              [Exception (vbody sbody)
                         (local [(define exn-args (hash-ref (VObjectClass-dict vbody) 'args))
=======
                                (Exception (Exception-v exn) hsto henv)
                                (CExcept-body (some-v match?)) stk)
                    (interp-env (some-v match?) henv hsto stk))])
            (type-case Result result
              [v*s*e (vbody sbody ebody) (v*s*e vnone sbody ebody)]
              [Return (vbody sbody ebody) (Return vbody sbody ebody)]
              [Break (sbody ebody) (Break sbody ebody)]
              [Exception (vbody sbody ebody)
                         (local [(define exn-args (hash-ref (VObject-dict vbody) 'args))
>>>>>>> master
                                 (define exn-args-val (if (some? exn-args)
                                                          (some (fetch (some-v exn-args) sbody))
                                                          (none)))
                                 (define exn-mval (if (some? exn-args-val)
                                                      (VObjectClass-mval (some-v exn-args-val))
                                                      (none)))
                                 (define exn-tuple (if (some? exn-mval)
                                                       (some (MetaTuple-v (some-v exn-mval)))
                                                       (none)))
                                 (define exn-tuple-mval (if (and (some? exn-tuple)
                                                                 (> (length (some-v exn-tuple))
                                                                    0))
                                                            (VObjectClass-mval
                                                              (first (some-v exn-tuple)))
                                                            (none)))
                                 (define exn-str (if (some? exn-tuple-mval)
                                                     (MetaStr-s (some-v exn-tuple-mval))
                                                     ""))]
                         (if (string=? exn-str "No active exception to reraise")
                             (Exception (Exception-v exn) sbody)
                             result))])))
        (Exception (Exception-v exn) hsto)))))

<<<<<<< HEAD
(define (interp-let [name : symbol] [type : IdType] [value : Result]
                    [body : CExpr] [env : Env]) : Result
  (local [(define-values (val sto mayb-loc)
            (type-case Result value
              [v*s (v s a) (values v s a)]
              [Return (v s a) (values v s a)]
              [Break (s) (values vnone s (none))]
              [Continue (s) (values vnone s (none))] 
              [Exception (v s) (values v s (none))]))
          (define loc (if (some? mayb-loc)
                          (some-v mayb-loc)
                          (new-loc)))
          (define newenv (cons (hash-set (first env) name loc) (rest env)))]
    (interp-env body
                ; Needed still?
                ; From Anand:
                #|(if (global-scope? env)
                    ; Creating a new localscope for handling CLet in global scope.
                    ; Assuming that there won't be any local bindings in the let body.
                    (cons (hash-set (hash (list)) name loc) env) 
                    (cons (hash-set (first env) name loc) (rest env)))|#
                newenv 
                (hash-set sto loc (set-class val env)))))

;; interp-env : CExpr * Env * Store -> Result
(define (interp-env [expr : CExpr] [env : Env] [sto : Store]) : Result
  (begin ;(display expr) (display "\n")
         ;(display env) (display "\n\n")
=======
(define (interp-let [name : symbol] [value : Result] [body : CExpr] [stk : Stack]) : Result
  (local [(define (interp-let/inner env store value)
            (let ([loc (new-loc)])
              (interp-env body
                          (if (global-scope? env)
                              ; Creating a new localscope for handling CLet in global scope.
                              ; Assuming that there won't be any local bindings in the let body.
                              (cons (hash-set (hash (list)) name loc) env) 
                              (cons (hash-set (first env) name loc) (rest env)))
                          (hash-set store loc value)
                          stk)))]
    (type-case Result value
      [v*s*e (vb sb eb) (interp-let/inner eb sb vb)]
      [Return (vb sb eb) (interp-let/inner eb sb vb)]
      [Break (sb eb) (interp-let/inner eb sb vnone)]
      [Exception (vb sb eb) (interp-let/inner eb sb vb)])))

;; the optional address whould be included in Result with the new scope.
(define (interp-cid [x : symbol] [t : IdType] 
                    [env : Env] [sto : Store] [stk : Stack]) : (Result * (optionof Address))
  (let ([result 
         (type-case IdType t
           [LocalId () 
                    (let ([local-w (lookup-local x env)])
                      (if (some? local-w)
                          (type-case CVal (fetch (some-v local-w) sto)
                            [VUndefined () (values 
                                            (mk-exception 'UnboundLocalError 
                                                         (string-append (symbol->string x)
                                                                        " is undefined in this scope")
                                                         env sto stk)
                                            (none))]
                            [else (values
                                   (v*s*e (fetch (some-v local-w) sto) sto env)
                                   local-w)])
                          (let ([full-w (lookup x env)]
                                [name-error-str (string-append "name '" 
                                                               (string-append (symbol->string x)
                                                                              "' is not defined"))])
                            (if (some? full-w)
                                (type-case CVal (fetch (some-v full-w) sto)
                                  [VUndefined () (values
                                                  (mk-exception 'NameError name-error-str env sto stk)
                                                  (none))]
                                  [else (values
                                         (v*s*e (fetch (some-v full-w) sto) sto env)
                                         full-w)])
                                (values 
                                 (mk-exception 'NameError name-error-str env sto stk)
                                 (none))))))]
           [NonlocalId ()
                       (local [(define nonlocal-w (lookup-nonlocal x env))
                               (define name-error-str
                                 (string-append "free variable '"
                                                (string-append (symbol->string x)
                                                               (string-append 
                                                                "' referenced before assignment "
                                                                "in enclosing scope"))))]
                         (if (some? nonlocal-w)
                             (local [(define nonlocal-val (fetch (some-v nonlocal-w) sto))]
                               (type-case CVal nonlocal-val
                                 [VUndefined () (values
                                                 (mk-exception 'NameError name-error-str env sto stk)
                                                 (none))]
                                 [else (values
                                        (v*s*e nonlocal-val sto env)
                                        nonlocal-w)]))
                             (local [(define syntax-error-str
                                       (string-append "no binding for nonlocal '"
                                                      (string-append (symbol->string x)
                                                                     "' found")))]
                               (values
                                (mk-exception 'SyntaxError syntax-error-str env sto stk)
                                (none)))))]
           [GlobalId ()
                     (local [(define full-w (lookup x env))
                             (define name-error-str
                               (string-append "name '"
                                              (string-append (symbol->string x)
                                                             "' is not defined")))]
                       (if (some? full-w)
                           (local [(define full-val (fetch (some-v full-w) sto))]
                             (type-case CVal full-val
                               [VUndefined () (values
                                               (mk-exception 'NameError name-error-str env sto stk)
                                               (none))]
                               [else (values
                                      (v*s*e full-val sto env)
                                      full-w)]))
                           (values 
                            (mk-exception 'NameError name-error-str env sto stk)
                            (none))))])])
    (if (symbol=? x 'x)
        (begin
          ;(display "id: ") (display x)
          ;(display ", val: ") (display (v*s*e-v result)) (display "\n")
          result)
        result)))

;; interp-env : CExpr * Env * Store * Stack -> Result
(define (interp-env [expr : CExpr] [env : Env] [sto : Store] [stk : Stack]) : Result
>>>>>>> master
  (type-case CExpr expr
    [CModule (prelude body)
             (local [(define prelude-r (interp-env prelude env sto stk))]
                (type-case Result prelude-r
<<<<<<< HEAD
                  [v*s (v s a) (interp-env body env s)]
                  [Return (v s a) (return-exception s)]
                  [Break (s) (break-exception s)]
                  [Continue (s) (continue-exception s)] 
                  [Exception (v s) (Exception v s)]))]
=======
                    [v*s*e (v s e) (interp-env body e s stk)]
                    [Return (v s e) (return-exception e s stk)]
                    [Break (s e) (break-exception e s stk)]
                    [Exception (v s e) (Exception v s e)]))]
>>>>>>> master
    
    [CStr (s) (v*s (VObject 'str (some (MetaStr s)) (hash empty)) sto (none))]
    [CTrue () (v*s true-val sto (none))]
    [CFalse () (v*s false-val sto (none))]
    [CNone () (v*s vnone sto (none))]
    [CUndefined () (v*s (VUndefined) sto (none))]

    [CClass (name bases body)
            ;; the tuple of bases is evaluated assuming global scope for class names,
            ;; should be changed for a more general lookup with the new scope implementation
<<<<<<< HEAD
            (begin ;(display "BEGIN CLASS\n") (display bases)
            (type-case Result (interp-env (CTuple (map (lambda (id) (CId id (GlobalId)))
                                                       bases))
                                          env sto)
              [v*s (vbases sbases abases)
                   (type-case Result (interp-env body (cons (hash empty) env) sto)
                     [v*s (vbody sbody abody)
                          (begin ;(display name) (display "\n")
                                 ;(display env) (display "\n")
                                 ;(display sbody) (display "\n")
                                 (let ([res (mk-type name vbases (hash empty) sbody env)])
                                   res))]
                   [Return (vval sval aval) (return-exception sval)]
                   [Break (sval) (break-exception sval)]
                   [Continue (sval) (continue-exception sval)] 
                   [Exception (vval sval) (Exception vval sval)])]
            [Return (vbases sbases abases) (return-exception sbases)]
            [Break (sbases) (break-exception sbases)]
            [Continue (sbases) (continue-exception sbases)] 
            [Exception (vbases sbases) (Exception vbases sbases)]))]
   
    [CGetField (value attr)
               (type-case Result (interp-env value env sto)
                          [v*s (vval sval aval) (get-field attr vval env sval)]
                          [Return (vval sval aval) (return-exception sval)]
                          [Break (sval) (break-exception sval)]
                          [Continue (sval) (continue-exception sval)] 
                          [Exception (vval sval) (Exception vval sval)])]
			
    [CSeq (e1 e2) (type-case Result (interp-env e1 env sto)
                    [v*s (v1 s1 a1) (interp-env e2 env s1)]
                    [Return (v1 s1 a1) (Return v1 s1 a1)]
                    [Break (s1) (Break s1)]
                    [Continue (s1) (Continue s1)] 
                    [Exception (v1 s1) (Exception v1 s1)])]
=======
            (type-case Result (interp-env (CTuple (map (lambda (id) (CId id (GlobalId))) bases)) env sto stk)
              [v*s*e (vbases sbases ebases)
               (type-case Result (interp-env body (cons (hash empty) env) sto stk)
                 [v*s*e (vbody sbody ebody)
                        (mk-type name vbases (first ebody) sbody env stk)]
                 [Return (vval sval eval) (return-exception eval sval stk)]
                 [Break (sval eval) (break-exception eval sval stk)]
                 [Exception (vval sval eval) (Exception vval sval eval)])]
              [Return (vval sval eval) (return-exception eval sval stk)]
              [Break (sval eval) (break-exception eval sval stk)]
              [Exception (vval sval eval) (Exception vval sval eval)])]
    
    [CGetField (value attr)
               (begin ;(display "CGetField: ") (display value) (display " . ") (display attr) (display "\n")
               (local
                 [(define-values (result w_value)
                    (type-case CExpr value
                      [CId (x t) 
                           (begin ;(display "->CId ") (display x) (display ": ")
                                  (interp-cid x t env sto stk))]
                      [else (values (interp-env value env sto stk) (none))]))]
                 (begin  ;(display w_value) (display "\n")
                 (type-case Result result
                   [v*s*e (vval sval eval)
                          (get-field attr vval w_value eval sval stk)]
                   [Return (vval sval eval) (return-exception eval sval stk)]
                   [Break (sval eval) (break-exception eval sval stk)]
                   [Exception (vval sval eval) (Exception vval sval eval)]))))]
			
    [CSeq (e1 e2) (type-case Result (interp-env e1 env sto stk)
                    [v*s*e (v1 s1 new-env) (interp-env e2 new-env s1 stk)]
                    [Return (v1 s1 new-env) (Return v1 s1 new-env)]
                    [Break (s1 new-env) (Break s1 new-env)]
                    [Exception (v1 s1 new-env) (Exception v1 s1 new-env)])]
>>>>>>> master
    
    ;; note that for now we're assuming that dict keys and values aren't going
    ;; to mess with the environment and store, but this might be wrong
    [CDict (contents)
           (letrec ([interped-hash (make-hash empty)]
                    [interp-pairs (lambda (lst)
                                  (map (lambda (pair)
                                       (hash-set! interped-hash
<<<<<<< HEAD
                                                   (v*s-v (interp-env (car pair) env sto))
                                                   (v*s-v (interp-env (cdr pair) env sto))))
=======
                                                   (v*s*e-v (interp-env (car pair) env sto stk))
                                                   (v*s*e-v (interp-env (cdr pair) env sto stk))))
>>>>>>> master
                                      lst))])
             (begin
               (interp-pairs (hash->list contents))
               (v*s (VObject '$dict
                              (some (MetaDict interped-hash))
                              (hash empty))
                    sto
                    (none))))]

    [CSet (elts)
<<<<<<< HEAD
          (local [(define-values (result-list new-s)
                                     (interp-cascade elts sto env))]
=======
          (local [(define-values (result-list new-s new-e)
                                     (interp-cascade elts sto env stk))]
>>>>>>> master
              (let ([exn? (filter Exception? result-list)])
                  (if (< 0 (length exn?))
                      (first exn?) 
                      (let ([val-list (map v*s-v result-list)])
                           (v*s (VObject 'set
                                         (some (MetaSet (make-set val-list)))
                                         (hash empty))
                                new-s
                                (none))))))]
    
    [CList (values)
<<<<<<< HEAD
           (local [(define-values (result-list new-s)
                                      (interp-cascade values sto env))]
=======
           (local [(define-values (result-list new-s new-e)
                                      (interp-cascade values sto env stk))]
>>>>>>> master
               (let ([exn? (filter Exception? result-list)])
                   (if (< 0 (length exn?))
                       (first exn?)
                       (let ([val-list (map v*s-v result-list)])
                         (v*s (VObject 'list
                                       (some (MetaList val-list))
                                       (hash empty))
                              new-s
                              (none))))))]

    [CTuple (values)
<<<<<<< HEAD
           (local [(define-values (result-list new-s)
                                      (interp-cascade values sto env))]
=======
           (local [(define-values (result-list new-s new-e)
                                      (interp-cascade values sto env stk))]
>>>>>>> master
               (let ([exn? (filter Exception? result-list)])
                   (if (< 0 (length exn?))
                       (first exn?) 
                      (let ([val-list (map v*s-v result-list)])
                       (v*s (VObject 'tuple
                                     (some (MetaTuple val-list))
                                     (hash empty))
                            new-s
                            (none))))))]

    ;; only for ids!
<<<<<<< HEAD
    [CAssign (t v) 
             (begin ;(display "ASSIGN: ") (display t) (display " | ") (display v) (display "\n")
             (local [(define val (interp-env v env sto))]
               (type-case Result val
                 [v*s (vv sv av)
                      (type-case CExpr t
                        [CId (x type) (assign-to-id t val env sv)]
                        [CGetField (o a) (assign-to-field o a val env sv)]
                        [else (mk-exception 'SyntaxError
                                            "can't assign to literals"
                                            sv)])]
                 [Return (vv sv av) (return-exception sv)]
                 [Break (sv) (break-exception sv)]
                 [Continue (sv) (continue-exception sv)] 
                 [Exception (vv sv) (Exception vv sv)])))]
    
    ;; is this used anymore?
    [CError (e) (type-case Result (interp-env e env sto)
                  [v*s (ve se ae) (raise-user-error (pretty ve))]
                  [Return (ve se ae) (return-exception se)]
                  [Break (se) (break-exception se)]
                  [Continue (se) (continue-exception se)] 
                  [Exception (ve se) (Exception ve se)])]

    [CIf (i t e) (type-case Result (interp-env i env sto)
                   [v*s (vi si ai) (if (truthy? vi)
                                       (interp-env t env si)
                                       (interp-env e env si))]
                   [Return (vi si ai) (return-exception si)]
                   [Break (si) (break-exception si)]
                   [Continue (si) (continue-exception si)] 
                   [Exception (vi si) (Exception vi si)])]
=======
    [CAssign (t v) (type-case Result (interp-env v env sto stk)
                     [v*s*e (vv sv venv)
                            (begin 
                              ;(if (and (CId? t) (symbol=? (CId-x t) 'x))
                              ;    (begin
                                    ;(display "assign: ") (display t) (display " ")
                                    ;(display vv) (display "\n")
                                    ;(display env) (display "\n\n"))
                              ;    (display ""))
                            (type-case CExpr t
                              [CId (x type) (assign-to-id t vv venv sv stk)]
                              [CGetField (o a) (assign-to-field o a vv venv sv stk)]
                              [else (mk-exception 'SyntaxError
                                                  "can't assign to literals"
                                                  venv
                                                  sv
                                                  stk)]))]
                     [Return (vv sv ev) (return-exception ev sv stk)]
                     [Break (sv ev) (break-exception ev sv stk)]
                     [Exception (vv sv ev) (Exception vv sv ev)])]
    
    [CIf (i t e) (type-case Result (interp-env i env sto stk)
                       [v*s*e (vi si envi) (if (truthy? vi)
                                             (interp-env t envi si stk)
                                             (interp-env e envi si stk))]
                       [Return (vi si envi) (return-exception envi si stk)]
                       [Break (si envi) (break-exception envi si stk)]
                       [Exception (vi si envi) (Exception vi si envi)])]

    [CId (x t)
         (local
           [(define-values (result w)
             (interp-cid x t env sto stk))]
           (begin ;(display "CId ") (display x) (display ": ") (display w) (display "\n")
           result))]
>>>>>>> master

    [CId (x type)
         (local [(define name-error-str
                   (string-append "name '"
                                  (string-append (symbol->string x)
                                                 "' is not defined")))
                 (define freevar-error-str
                   (string-append "free variable '"
                                  (string-append (symbol->string x)
                                                 "' referenced before assignment in enclosing scope")))
                 (define unboundlocal-error-str
                   (string-append "local variable '"
                                  (string-append (symbol->string x)
                                                 "' referenced before assignment")))]
           (type-case IdType type
             [LocalId () 
                      (local [(define local-w (lookup-local x env))]
                        (if (some? local-w)
                            (type-case CVal (fetch (some-v local-w) sto)
                              [VUndefined () (mk-exception 'UnboundLocalError
                                                           unboundlocal-error-str
                                                           sto)]
                              [else (v*s (fetch (some-v local-w) sto) sto local-w)])
                            (local [(define full-w (lookup x env))]
                              (if (some? full-w)
                                  (local [(define full-val (fetch (some-v full-w) sto))]
                                    (type-case CVal full-val
                                      [VUndefined () (mk-exception 'NameError freevar-error-str sto)]
                                      [else
                                       (if (immutable-type? full-val)
                                           (v*s full-val sto (none)) ; immutable return (none) as alias
                                           (v*s full-val sto full-w))]))
                                  (mk-exception 'NameError
                                                (string-append "global " name-error-str)
                                                sto)))))]
             [GlobalId ()
                       (local [(define full-w (lookup-global x env))]
                         (if (some? full-w)
                             (local [(define full-val (fetch (some-v full-w) sto))]
                               (type-case CVal full-val
                                 [VUndefined () (mk-exception 'NameError name-error-str sto)]
                                 [else
                                  (if (immutable-type? full-val)
                                      (v*s full-val sto (none)) ; immutable return (none) as alias
                                      (v*s full-val sto full-w))]))
                             (mk-exception 'NameError name-error-str sto)))]))]
    
    [CObject (c mval) (v*s (VObject c mval (hash empty)) sto (none))]

<<<<<<< HEAD
    [CLet (x type bind body)
          (begin ;(display "LET: ") (display x) (display " ")
                 ;(display type) (display bind) (display "\n")
          (interp-let x type (interp-env bind env sto) body env))]
=======
    [CLet (x bind body)
          (let ([w (new-loc)]
                [result (interp-env bind env sto stk)])
            (interp-let x result body stk))]
>>>>>>> master

    [CApp (fun arges sarg)
          (begin ;(display fun) (display arges)
          (interp-capp fun arges
                       (if (none? sarg)
<<<<<<< HEAD
                           (some (CTuple empty))
                           sarg)
                       env sto))]

    [CFunc (args sargs body method?) 
           (v*s (VClosure
                  (cons (hash empty) env)
                  args sargs body)
                sto
                (none))]

    [CReturn (value)
             (type-case Result (interp-env value env sto)
               [v*s (vv sv av) (Return vv sv av)]
               [Return (vv sv av) (return-exception sv)]
               [Break (sv) (break-exception sv)]
               [Continue (sv) (continue-exception sv)] 
               [Exception (vv sv) (Exception vv sv)])]

    [CPrim1 (prim arg)
            (type-case Result (interp-env arg env sto)
              [v*s (varg sarg aarg) 
                   (case prim
                     ['Not (if (truthy? varg)
                             (v*s false-val sarg (none))
                             (v*s true-val sarg (none)))]
                     [else (v*s (python-prim1 prim varg) sarg (none))])]
              [Return (varg sarg aarg) (return-exception sarg)]
              [Break (sarg) (break-exception sarg)]
              [Continue (sarg) (continue-exception sarg)] 
              [Exception (varg sarg) (Exception varg sarg)])]

    [CWhile (body test orelse) (interp-while body test orelse env sto)]
=======
                         (some (CTuple empty))
                         sarg)
                       env
                       sto
                       stk))]
                                  (begin ;(display "result: ") 
                                         ;(display (v*s*e-v result)) (display "\n\n")
                                         result))]

    [CFunc (args sargs body opt-class) (begin ;(display "func ") (display env) (display "\n\n")
           (v*s*e (VClosure (cons (hash empty) (if (some? opt-class)
                                                   (rest env)
                                                   env))
                            args sargs body opt-class)
                  sto env))]

    [CReturn (value) (type-case Result (interp-env value env sto stk)
                       [v*s*e (vv sv ev) (Return vv sv ev)]
                       [Return (vv sv ev) (return-exception ev sv stk)]
                       [Break (sv ev) (break-exception ev sv stk)]
                       [Exception (vv sv ev) (Exception vv sv ev)])]

    [CPrim1 (prim arg)
            (type-case Result (interp-env arg env sto stk)
              [v*s*e (varg sarg envarg) 
                   (case prim
                     ['Not (if (truthy? varg)
                             (v*s*e false-val sarg envarg)
                             (v*s*e true-val sarg envarg))]
                     [else (v*s*e (python-prim1 prim varg) sarg envarg)])]
              [Return (varg sarg earg) (return-exception earg sarg stk)]
              [Break (sarg earg) (break-exception earg sarg stk)]
              [Exception (varg sarg earg) (Exception varg sarg earg)])]

    [CWhile (body test orelse) (interp-while body test env sto stk)]
>>>>>>> master
    
    ;; implement this
    [CPrim2 (prim arg1 arg2) (interp-cprim2 prim arg1 arg2 sto env stk)]
    
<<<<<<< HEAD
    [CBuiltinPrim (op args)
                  (local [(define-values (result-list new-s)
                            (interp-cascade args sto env))
                          (define exn? (filter Exception? result-list))]
                    (if (< 0 (length exn?))
                        (first exn?)
                        (local [(define val-list (map v*s-v result-list))
                                (define mayb-val (builtin-prim op val-list env new-s))] 
                          (if (some? mayb-val)
                              ;; BuiltinPrims should return Results, not (optionof CVal)
                              (v*s (some-v mayb-val) new-s (none))
                              ;; todo: more useful errors
                              (mk-exception 'TypeError "Bad types in builtin call" 
                                            sto)))))]
    [CRaise (expr) 
            (if (some? expr)
                (type-case Result (interp-env (some-v expr) env sto)
                  [v*s (vexpr sexpr aexpr)
                       (cond
                         [(and (VObjectClass? vexpr) (object-is? vexpr 'Exception env sto))
                          (Exception vexpr sexpr)]
                         [else (mk-exception 'TypeError
                                             "exceptions must derive from BaseException"
                                             sexpr)])]
                  [Return (vexpr sexpr aexpr) (return-exception sexpr)]
                  [Break (sexpr) (break-exception sexpr)]
                  [Continue (sexpr) (continue-exception sexpr)] 
                  [Exception (vexpr sexpr) (Exception vexpr sexpr)])
                (mk-exception 'RuntimeError
                              "No active exception to reraise"
                              sto))]
    
    [CTryExceptElseFinally (try excepts orelse finally)
         (type-case Result (interp-env try env sto)
            [v*s (vtry stry atry)
                   (type-case Result (interp-env orelse env stry)
                      [v*s (velse selse aelse)
                             (type-case Result (interp-env finally env selse)
                                [v*s (vfin sfin afin)
                                       (v*s vnone sfin (none))]
                                [Return (vfin sfin afin) (return-exception sfin)]
                                [Break (sfin) (Break sfin)]
                                [Continue (sfin) (Continue sfin)] 
                                [Exception (vfin sfin) (Exception vfin sfin)])]
                      [Return (velse selse aelse) (return-exception selse)]
                      [Break (selse) (Break selse)]
                      [Continue (selse) (Continue selse)]
                      [Exception (velse selse) (Exception velse selse)])]
            [Return (vtry stry atry) (Return vtry stry atry)]
            [Break (stry) (Break stry)]
            [Continue (stry) (Continue stry)]
=======
    [CBuiltinPrim (op args) (local [(define-values (result-list new-s new-e)
                                      (interp-cascade args sto env stk))]
                               (let ([exn? (filter Exception? result-list)])
                                 (if (< 0 (length exn?))
                                     (first exn?)
                                     (let ([val-list (map v*s*e-v result-list)])
                                       (local [(define mayb-val 
                                               (builtin-prim op val-list new-e new-s stk))] 
                                              (if (some? mayb-val)
                                       (v*s*e (some-v mayb-val)
                                              new-s
                                              new-e)
                                       ;; todo: more useful errors
                                       (mk-exception 'TypeError "Bad types in builtin call" env
                                                     sto stk)))))))]
    [CRaise (expr) 
            (if (some? expr)
                (type-case Result (interp-env (some-v expr) env sto stk)
                  [v*s*e (vexpr sexpr eexpr)
                         (cond
                           [(and (VObject? vexpr) (object-is? vexpr 'Exception env sto))
                            (Exception vexpr sexpr eexpr)]
                           [else (mk-exception 'TypeError
                                               "exceptions must derive from BaseException"
                                               eexpr
                                               sexpr stk)])]
                  [Return (vexpr sexpr eexpr) (return-exception eexpr sexpr stk)]
                  [Break (sexpr eexpr) (break-exception eexpr sexpr stk)]
                  [Exception (vexpr sexpr eexpr) (Exception vexpr sexpr eexpr)])
                (mk-exception 'RuntimeError
                              "No active exception to reraise"
                              env sto stk))]
    
    [CTryExceptElseFinally (try excepts orelse finally)
         (type-case Result (interp-env try env sto stk)
            [v*s*e (vtry stry etry)
                   (type-case Result (interp-env orelse etry stry stk)
                      [v*s*e (velse selse eelse)
                             (type-case Result (interp-env finally eelse selse stk)
                                [v*s*e (vfin sfin efin)
                                       (v*s*e vnone sfin efin)]
                                [Return (vfin sfin efin) (return-exception efin sfin stk)]
                                [Break (sfin efin) (Break sfin efin)]
                                [Exception (vfin sfin efin)
                                           (Exception vfin sfin efin)])]
                      [Return (velse selse eelse) (return-exception eelse selse stk)]
                      [Break (selse eelse) (Break selse eelse)]
                      [Exception (velse selse eelse)
                                 (Exception velse selse eelse)])]
            [Return (vtry stry etry) (Return vtry stry etry)]
            [Break (stry etry) (Break stry etry)]
>>>>>>> master
            ;; handle excepts here
            [Exception (vtry stry)
               (local [(define result 
                         (if (empty? excepts)
<<<<<<< HEAD
                             (Exception vtry stry)
                             (interp-excepts excepts stry env
                                             (Exception vtry stry))))]
                 ;; TODO: make sure this is still right!
                 (if (not (v*s? result))
                     (begin
                       (interp-env finally env (Exception-s result))
                       result)
                     (interp-env finally env (v*s-s result))))])]
=======
                             (Exception vtry stry etry)
                             (interp-excepts excepts stry etry
                                             (Exception vtry stry etry) stk)))]
                 (type-case Result result
                   [v*s*e (vexc sexc eexc) 
                          (interp-env finally eexc sexc stk)]
                   ;; finally block is excecuted, but side effects are ignored...
                   [Exception (vexc sexc eexc)
                              (begin
                                (interp-env finally eexc sexc stk)
                                result)]
                   [Return (vexc sexc eexc)
                           (begin
                             (interp-env finally eexc sexc stk)
                             result)]
                   [Break (sexc eexc)
                          (begin
                            (interp-env finally eexc sexc stk)
                            result)]))])]
>>>>>>> master

    [CExcept (types name body) (interp-env body env sto stk)]
    
    [CBreak () (Break sto)]
    [CContinue () (Continue sto)]
    )))

(define (assign-to-id [id : CExpr] [val : Result] [env : Env] [sto : Store]) : Result
  (local [(define mayb-loc 
            (type-case IdType (CId-type id)
              [LocalId () (lookup (CId-x id) env)]
              [GlobalId () (lookup-global (CId-x id) env)]))
          (define value (if (v*s? val)
                            (if (some? (v*s-a val))
                                (VPointer (some-v (v*s-a val)))
                                (v*s-v val))
                            (if (some? (Return-a val))
                                (VPointer (some-v (Return-a val)))
                                (Return-v val))))]
    (if (some? mayb-loc)
        (v*s vnone (hash-set sto (some-v mayb-loc) (set-class value env)) (none))        
        (type-case IdType (CId-type id)
          [LocalId () (mk-exception 'NameError
                                  (string-append "name '"
                                    (string-append (symbol->string (CId-x id))
                                      "' is not defined"))
                                    sto)]
          [GlobalId () (mk-exception 'NameError
                                   (string-append "name '"
                                     (string-append (symbol->string (CId-x id))
                                       "' is not defined"))
                                   sto)]))))

(define (global-scope? [env : Env]) : boolean
  (= (length env) 1))

<<<<<<< HEAD
;; handles lookup chain for function calls on objects
;; multiple inheritance modification : for class lookup call get-field-from-class
(define (get-field [n : symbol] [c : CVal] [e : Env] [s : Store]) : Result
  (begin ;(display "GET: ") (display n) (display " FROM:") (display c) (display "\n")
         ;(display e) (display "\n\n")
    ;(display e) (display "\n\n")
    (cond 
      [(and (some? (VObjectClass-mval c)) (MetaClass? (some-v (VObjectClass-mval c))))
=======
(define (assign-to-id id v e s stk)
  (local [(define-values (before scope after global? error)
            (type-case IdType (CId-type id)
              [GlobalId () (values (drop-right e 1) (last e) empty #t (none))]
              [NonlocalId ()
                (local [(define (find-scope-level [x : symbol] [env : Env] [idx : number])
                          (cond
                            [(empty? (rest env)) (none)]
                            [else (if (some? (hash-ref (first env) x))
                                      (some idx)
                                      (find-scope-level x (rest env) (add1 idx)))]))
                        (define level-idx (find-scope-level (CId-x id) (rest e) 1))]
                  (if (none? level-idx)
                      (values empty (hash empty) empty #f
                              (some
                                (mk-exception 'SyntaxError
                                   (string-append "no binding for nonlocal '"
                                                  (string-append (symbol->string (CId-x id))
                                                                 "' found"))
                                   e s stk)))
                       (local [(define-values (left right)
                                 (values (take e (some-v level-idx))
                                         (drop e (some-v level-idx))))]
                        (begin ;(display "left: ") (display left) (display "\n")
                               ;(display "right: ") (display right) (display "\n")
                              (values left (first right) (rest right) #f (none))))))]
              [LocalId () (values empty (first e) (rest e) (global-scope? e) (none))]))]
    (if (some? error)
        (some-v error)
        (local [(define mayb-w (hash-ref scope (CId-x id)))
                (define w (if (some? mayb-w) (some-v mayb-w) (new-loc)))
                (define (set-local scope id loc)
                  (hash-set scope (CId-x id) loc))
                (define (set-global scope id loc)
                  (begin
                    (hash-set! scope (CId-x id) loc)
                    scope))]
          (begin 
            ;(if (symbol=? 'x (CId-x id))
            ;  (begin
            ;     (display scope) (display "\n")
            ;     (display w) (display "\n\n"))
            ;  (display ""))
          (v*s*e vnone
                 (hash-set s w v) 
                 (append3
                   before
                   (list (if global? (set-global scope id w)
                                     (set-local scope id w)))
                   after)))))))

;; handles lookup chain for function calls on objects
;; multiple inheritance modification : for class lookup call get-field-from-class
;; optional address field added to support self aliasing in bound methods calls.
(define (get-field [n : symbol] [c : CVal] [w_c : (optionof Address)] 
                   [e : Env] [s : Store] [stk : Stack]) : Result
  (begin ;(display "GET: ") (display n) (display " ") (display c) 
         ;(display " ") (display w_c) (display "\n")
         ;(display e) (display "\n\n")
    (cond
      [(not (VObject? c))
       (mk-exception 'AttributeError
                     (string-append 
                      (string-append (pretty c) " object has no attribute ")
                      (symbol->string n))
                     e s stk)]
      [(is-special-method? n)
       ;; special methods are looked for in the class
       (get-field-from-obj n c w_c (none) e s stk)]
      [(and (some? (VObject-mval c)) (MetaClass? (some-v (VObject-mval c))))
>>>>>>> master
       ;; class lookup
       (get-field-from-cls n c w_c (none) e s stk)]
      [else
<<<<<<< HEAD
        ;; instance lookup
        (type-case CVal c
          [VObjectClass (antecedent mval d class) 
                   (let ([w (hash-ref (VObjectClass-dict c) n)])
                     (begin ;(display "loc: ") (display w) (display "\n\n")
                       (type-case (optionof Address) w
                         [some (w) 
                               (v*s (fetch w s) s (some w))]
                         [none () 
                               (local [(define __class__w class)]
                                 (type-case (optionof Address) __class__w
                                   [some (w)
                                         (get-field-from-class n
                                                               (fetch (some-v __class__w) s)
                                                               e s)]
                                   [none () (let ([mayb-base (lookup antecedent e)])
                                              (if (some? mayb-base)
                                                  (let ([base (fetch (some-v mayb-base) s)])
                                                    (get-field-from-class n base e s))
                                                  (error 'get-field (string-append 
                                                                      "Object without class: "
                                                                      (pretty c)))))]))])))]
          [else (error 'interp "Not an object with fiedls.")])])))

(define (assign-to-field o f v e s)
  (begin ;(display o) (display "---") (display f) (display "\n")
  (type-case Result (interp-env o e s)
    [v*s (vo so ao)
         (type-case CVal vo
           [VObjectClass (ante-name mval d class)
             (local [(define loc (hash-ref (VObjectClass-dict vo) f))
                     (define value (set-class 
                                     (if (v*s? v)
                                         (if (some? (v*s-a v))
                                             (VPointer (some-v (v*s-a v)))
                                             (v*s-v v))
                                         (if (some? (Return-a v))
                                             (VPointer (some-v (Return-a v)))
                                             (Return-v v)))
                                     e))]
               (type-case (optionof Address) loc
                 [some (w) (v*s vnone (hash-set so w value) (none))]
                 [none () (local [(define w (new-loc))
                                  (define objw (if (some? ao)
                                                 (some-v ao)
                                                 (new-loc)))
                                  (define snew
                                    (begin ;(display vo) (display "\n")
                                           ;(display objw) (display "\n")
                                    (hash-set so objw 
                                              (VObject ante-name
                                                       mval
                                                       (hash-set (VObjectClass-dict vo) f w)))))]
                              (v*s vnone
                                   (hash-set snew w value)
                                   (none)))]))]
           [else (error 'interp "Can't assign to nonobject.")])]
    [Return (vo so ao) (return-exception so)]
    [Break (so) (break-exception so)]
    [Continue (so) (continue-exception so)] 
    [Exception (vo so) (Exception vo so)])))
=======
       ;; instance lookup
       (type-case CVal c
         [VObject (antecedent mval d) 
                  (let ([w (hash-ref (VObject-dict c) n)])
                    (begin ;(display "loc: ") (display w) (display "\n\n")
                      (type-case (optionof Address) w
                        [some (w) 
                              (v*s*e (fetch w s) s e)]
                        [none () 
                              (get-field-from-obj n c w_c (none) e s stk)])))]
         [else (error 'interp "Not an object with functions.")])])))


(define (assign-to-field o f v e s stk)
  (type-case Result (interp-env o e s stk)
    [v*s*e (vo so eo) (type-case CVal vo
	[VObject (ante-name mval d)
	  (let ([w (hash-ref (VObject-dict vo) f)])
	    (type-case (optionof Address) (hash-ref (VObject-dict vo) f)
	      [some (w) (v*s*e vnone (hash-set so w v) eo)]
	      [none () (let ([w (new-loc)])
			   (let ([nw (hash-ref (first eo) (CId-x o))])
                  (let ([snew (hash-set so (some-v nw) 
			                        (VObject ante-name
                                       mval
						                           (hash-set (VObject-dict vo) f w)))])
      		           	(v*s*e vnone
                             (hash-set snew w v)
			                       eo))))]))]
    	[else (error 'interp "Can't assign to nonobject.")])]
    [Return (vo so eo) (return-exception eo so stk)]
    [Break (so eo) (break-exception eo so stk)]
    [Exception (vo so eo) (Exception vo so eo)]))
>>>>>>> master

(define (new-object [c-name : symbol] [c-loc : (optionof Address)]) : CVal
  (VObjectClass c-name (none) (hash empty) c-loc))

(define (set-class [val : CVal] [env : Env]) : CVal
  (begin ;(display val) (display "\n")
  (type-case CVal val
    [VObjectClass (antecedent mval dict class)
                  (if (none? class)
                      (if (none? (lookup antecedent env))
                          (error 'interp (string-append
                                           "Class not in environment: "
                                           (symbol->string antecedent)))
                          (VObjectClass antecedent mval dict (lookup antecedent env)))
                      val)]
    [else val])))

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
<<<<<<< HEAD
                   [sto : Store]) : (Env * Store * (optionof Result))
  (cond [(and (empty? args) (empty? vals))
         ;; This means we've bound all of the values to the argument symbols,
         ;; so we want to bind an empty tuple to the stararg symbol if there is one.
         (if (some? sarg)
           (bind-args (list (some-v sarg))
                      (none)
                      (list (v*s (make-builtin-tuple empty) sto (none)))
                      (list (make-builtin-num 0))
                      env 
                      ext
                      sto)
           (values ext sto (none)))]
=======
                   [sto : Store] [stk : Stack]) : (Env * Store * (optionof Result))
  (cond [(and (empty? args) (empty? vals)) 
              (if (some? sarg)
                  (bind-args (list (some-v sarg))
                             (none)
                             (list (make-builtin-tuple empty))
                             (list (make-builtin-num 0))
                             env 
                             ext
                             sto
                             stk)
                  (values ext sto (none)))]
        ;need to bind star args!
>>>>>>> master
        [(and (empty? args) (some? sarg)) 
         ;; This means we have a stararg symbol and extra arguments, so we'll
         ;; place them in a tuple and bind it to the stararg symbol.
         (let ([star-tuple (make-builtin-tuple (map v*s-v vals))])
           (bind-args (list (some-v sarg))
                      (none) 
<<<<<<< HEAD
                      (list (v*s star-tuple sto (none)))
                      (list (make-builtin-num 0))
                      env ext sto))]
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
                                         (to-string vals))
                                   "")
                                 sto)))]
        [(and (cons? args) (cons? vals))
         ;; This means we're still binding values to argument symbols.
         ;; If the object is mutable, we want to use its current store location.
         ;; If the object is immutable, we can make a new store location.
         (local [(define val (first vals))
                 (define-values (vv av) (values (v*s-v val) (v*s-a val)))
                 (define loc
                   (if (some? av)
                       (type-case CVal vv
                         [VObjectClass (ante-name mayb-mval dict class)
                                  (if (some? mayb-mval)
                                      (type-case MetaVal (some-v mayb-mval)
                                        [MetaList (l) (some-v av)]
                                        [MetaDict (d) (some-v av)]
                                        [MetaSet (s) (some-v av)]
                                        [else (new-loc)])
                                      (some-v av))]
                         [else (new-loc)])
                       (new-loc)))
                 (define e (cons (hash-set (first ext) (first args) loc) (rest ext)))
                 (define s (hash-set sto loc (set-class vv env)))]
           (bind-args (rest args) sarg (rest vals) (rest arges) env e s))]))

(define (mk-exception [type : symbol] [arg : string] [sto : Store]) : Result
  (local [(define loc (new-loc))
          (define args (list (VObject 'str (some (MetaStr arg)) (hash empty))))]
    (Exception
      (VObject type (none) (hash-set (hash empty) 'args loc))
      (hash-set sto loc (VObject 'tuple (some (MetaTuple args)) (hash empty))))))

(define (return-exception [sto : Store]) : Result
  (mk-exception 'SyntaxError "'return' outside function" sto))

(define (break-exception [sto : Store]) : Result
  (mk-exception 'SyntaxError "'break' outside loop" sto))
=======
                      (list star-tuple)
                      arges env ext sto stk))]


        [(or (empty? args) (empty? vals))
         (values ext sto (some (mk-exception 'TypeError 
                                             (string-join
                                               (list "Arity mismatch: "
                                                     "expected "
                                                     (to-string args)
                                                     ", received "
                                                     (to-string vals))
                                               "")
                                             env
                                             sto
                                             stk)))]
        [(and (cons? args) (cons? vals))
         (let ([val (first vals)]
               [where -1]
               [mutability-check (lambda ()
                    (type-case CExpr (first arges)
                         [CId (x t)
                              (if (symbol=? x 'init)
                                  (new-loc)
                                  (some-v (lookup x env)))]
                         [else (new-loc)]))])
            (begin
              (type-case CVal val
              [VObject (ante-name mayb-mval dict)
                       ;; these should not get new store locations if they already exist
                       (if (some? mayb-mval)
                         (let ([mval (some-v mayb-mval)])
                           (type-case MetaVal mval
                             [MetaClass (c) (set! where (mutability-check))]
                             [MetaList (l) (set! where (mutability-check))]
                             [MetaDict (d) (set! where (mutability-check))]
                             ;; immutable types should get a new store location
                             [else (set! where (new-loc))]))
                         (set! where (mutability-check)))]
              [else (set! where (new-loc))])
            (let ([e (cons (hash-set (first ext) (first args) where) (rest ext))]
                  [s (hash-set sto where (first vals))])
                 (bind-args (rest args) sarg (rest vals) (rest arges) env e s stk))))]))


(define (mk-exception [type : symbol] [arg : string]
                      [env : Env] [sto : Store] [stk : Stack]) : Result
  (let ([exception 
          (interp-env (make-exception
                        type 
                        arg)
                      env sto stk)])
    (if (or (Exception? exception) (Break? exception))
               exception
               (Exception (v*s*e-v exception)
                          (v*s*e-s exception)
                          (v*s*e-e exception)))))

(define (return-exception [env : Env] [sto : Store] [stk : Stack]) : Result
  (mk-exception 'SyntaxError "'return' outside function" env sto stk))

(define (break-exception [env : Env] [sto : Store] [stk : Stack]) : Result
  (mk-exception 'SyntaxError "'break' outside loop" env sto stk))
>>>>>>> master

(define (continue-exception [sto : Store]) : Result
  (mk-exception 'SyntaxError "'continue' outside loop" sto))

(define (interp expr)
<<<<<<< HEAD
  (type-case Result (interp-env expr (list (#|make-|#hash (list))) (hash (list)))
    [v*s (vexpr sexpr aexpr) (display "")]
    [Return (vexpr sexpr aexpr)
            (local [(define exn (return-exception sexpr))]
              (raise-user-error (string-append
                                  (pretty-exception (Exception-v exn)
                                                    (Exception-s exn))
                                  "\n")))]
    [Break (sexpr)
           (local [(define exn (break-exception sexpr))]
             (raise-user-error (string-append
                                 (pretty-exception (Exception-v exn)
                                                   (Exception-s exn))
                                 "\n")))]
    [Continue (sexpr)
           (local [(define exn (continue-exception sexpr))]
             (raise-user-error (string-append
                                 (pretty-exception (Exception-v exn)
                                                   (Exception-s exn))
                                 "\n")))] 
    [Exception (vexpr sexpr)
               (raise-user-error (string-append (pretty-exception vexpr sexpr) "\n"))]))

(define (truthy? [val : CVal]) : boolean
  (type-case CVal val
    [VClosure (e a s b) true]
    [VObjectClass (a mval d class) (truthy-object? (VObject a mval d))]
    [VUndefined () false]
    [else (error 'truthy? "Shouldn't check truthiness of Pointer.")]))
=======
  (type-case Result (interp-env expr (list (make-hash (list))) (hash (list)) (list))
    [v*s*e (vexpr sexpr env) (if (not (MetaNone? (some-v (VObject-mval vexpr))))
                         (begin (display (pretty vexpr)) 
                                (display "\n"))
                         (display ""))]
    [Return (vexpr sexpr env) (raise-user-error "SyntaxError: 'return' outside function")]
    [Break (sexpr env) (raise-user-error "SyntaxError: 'break' outside loop")]
    [Exception (vexpr sexpr env) (raise-user-error
                                   (pretty-exception vexpr sexpr))]))

(define (truthy? [val : CVal]) : boolean
  (type-case CVal val
    [VClosure (e a s b o) true]
    [VObject (a mval d) (truthy-object? (VObject a mval d))]
    [VUndefined () false]))
>>>>>>> master

(define (interp-cprim2 [prim : symbol] 
                       [arg1 : CExpr]
                       [arg2 : CExpr]
                       [sto : Store]
<<<<<<< HEAD
                       [env : Env]) : Result
    (type-case Result (interp-env arg1 env sto)
      [v*s (varg1 sarg1 aarg1)
           (type-case Result (interp-env arg2 env sarg1)
             [v*s (varg2 sarg2 aarg2) 
=======
                       [env : Env]
                       [stk : Stack]) : Result

    (type-case Result (interp-env arg1 env sto stk)
      [v*s*e (varg1 sarg1 envarg1)
           (type-case Result (interp-env arg2 envarg1 sarg1 stk)
             [v*s*e (varg2 sarg2 envarg2) 
>>>>>>> master
                  (case prim
                    ;; Handle Is, IsNot, In, NotIn
                    ['Is (if (is? varg1 varg2)
                           (v*s true-val sarg2 (none))
                           (v*s false-val sarg2 (none)))]
                    ['IsNot (if (not (is? varg1 varg2))
                           (v*s true-val sarg2 (none))
                           (v*s false-val sarg2 (none)))]
                    [else (error 'interp (string-append "Haven't implemented a case yet: "
                                                        (symbol->string
                                                          prim)))])]
<<<<<<< HEAD
             [Return (varg2 sarg2 aarg2) (return-exception sarg2)]
             [Break (sarg2) (break-exception sarg2)]
             [Continue (sarg2) (continue-exception sarg2)] 
             [Exception (varg2 sarg2) (Exception varg2 sarg2)])]
      [Return (varg1 sarg1 aarg1) (return-exception sarg1)]
      [Break (sarg1) (break-exception sarg1)]
      [Continue (sarg1) (continue-exception sarg1)] 
      [Exception (varg1 sarg1) (Exception varg1 sarg1)]))
=======
             [Return (varg2 sarg2 envarg2) (return-exception envarg2 sarg2 stk)]
             [Break (sarg2 envarg2) (break-exception envarg2 sarg2 stk)]
             [Exception (varg2 sarg2 envarg2) (Exception varg2 sarg2 envarg2)])]
      [Return (varg1 sarg1 envarg1) (return-exception envarg1 sarg1 stk)]
      [Break (sarg1 envarg1) (break-exception envarg1 sarg1 stk)]
      [Exception (varg1 sarg1 envarg1) (Exception varg1 sarg1 envarg1)]))
>>>>>>> master

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; multiple inheritance ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; mk-type will compute __mro__ for the class,
;; may return an exception if linearization is not possible
;; builtins/type.rkt would be a good place for this stuff,
;; it should handle type(name, bases, dict)
;; but it needs access to mk-exception...
(define (mk-type [name : symbol]
                 [bases : CVal]
                 [h-dict : (hashof symbol Address)]
                 [sto : Store]
<<<<<<< HEAD
                 [env : Env]) : Result
  (local [(define bases-list (MetaTuple-v (some-v (VObjectClass-mval bases))))
          (define w (new-loc))
          (define bases_w (new-loc))
          (define mro_w (new-loc))
          (define class_loc (lookup name env))
          (define class_w (if (some? class_loc)
                              (some-v class_loc)
                              (error 'mk-type
                                     (string-append "Class '"
                                       (string-append (symbol->string name)
                                         "' does not have a location in the store")))))]
    (type-case Result (build-mro name bases-list env sto)
      [v*s (vmro smro amro) 
             (v*s (VObject 'type
                           (some (MetaClass name)) 
                           (hash-set
=======
                 [env : Env]
                 [stk : Stack]) : Result
  (local [(define bases-list (MetaTuple-v (some-v (VObject-mval bases))))
          (define w (new-loc))
          (define bases_w (new-loc))
          (define mro_w (new-loc))]
    (type-case Result (build-mro name bases-list env sto stk)
      [v*s*e (vmro smro emro) 
             (v*s*e (VObject 'type
                             (some (MetaClass name)) 
>>>>>>> master
                             (hash-set 
                               (hash-set 
                                 (hash-set h-dict '__dict__ w)
                                 '__bases__ bases_w)
                               '__mro__ mro_w)
                             '__class__ class_w))
                  (hash-set 
                    (hash-set 
                      (hash-set sto w (make-under-dict h-dict sto))
                      bases_w bases)
<<<<<<< HEAD
                    mro_w vmro)
                  (none))]
      [Return (vmro smro amro) (return-exception smro)]
      [Break (smro) (break-exception smro)]
      [Continue (smro) (continue-exception smro)] 
      [Exception (vmro smro) (Exception vmro smro)])))
=======
                     mro_w vmro)
                    env)]
      [Return (vmro smro emro) (return-exception emro smro stk)]
      [Break (smro emro) (break-exception emro smro stk)]
      [Exception (vmro smro emro) (Exception vmro smro emro)])))
>>>>>>> master

;; build-mro: merge the __mro__ of the bases using the C3 algorithm
;; Raises TypeError if there are duplicated bases or linearization is not possible.
;; The class should be the first element of __mro__, but since this seems hard
;; to implement with immutable hashes, it will be prepended on retrieval
(define (build-mro [name : symbol] 
                   [bases : (listof CVal)] 
                   [env : Env] 
                   [sto : Store]
                   [stk : Stack]) : Result
  ;; The mro is the c3-merge of the mro of the bases plus the list of bases
<<<<<<< HEAD
  (begin ;(display "BASES for ") (display name) (display ": ") (display bases) (display "\n")
  (let ([maybe-mro (c3-merge (append (map (lambda (base) (get-mro base sto)) 
=======
  (let ([maybe-mro (c3-merge (append (map (lambda (base) (get-mro base (none) sto)) 
>>>>>>> master
                                          bases)
                                     (list bases)) empty)])
    (cond
      [(< (length (remove-duplicates bases)) (length bases))
       (mk-exception 'TypeError
                     (string-append 
                      "duplicate base class in class "
                      (symbol->string name))
<<<<<<< HEAD
                     sto)]
=======
                     env
                     sto
                     stk)]
>>>>>>> master
      [(none? maybe-mro) 
       (mk-exception 'TypeError
                     (string-append 
                      "cannot create a consisten method resolution order for class "
                      (symbol->string name))
<<<<<<< HEAD
                     sto)]
=======
                     env
                     sto
                     stk)]
>>>>>>> master
      [(some? maybe-mro)
       (begin 
         ;(display "class: ") (display name) (display " mro: ") 
         ;(display (map pretty (some-v maybe-mro))) (display "\n")
         (v*s (VObject 'tuple (some (MetaTuple (some-v maybe-mro))) (hash empty))
              sto
              (none)))]))))
 
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

(define ex1 (list (list 'o)))
(test (c3-merge ex1 empty) (some (list 'o)))
(define ex2 (list (list 'a 'o) 
                  (list 'o)))
(define ex3 (list (list 'a 'o)
                  (list 'b 'a 'o)
                  (list 'a 'b)))
(test (c3-merge ex3 empty) (none))
(define ex4 (list (list 'a 'o)
                  (list 'c 'a 'o)
                  (list 'b 'a 'o)
                  (list 'a 'c 'b)))
(test (c3-merge ex4 empty) (none))
(test (c3-merge ex2 empty) (some (list 'a 'o)))
(define ex5 (list (list 'd 'c 'b 'a 'o)
                  (list 'c 'a 'o)
                  (list 'b 'a 'o)
                  (list 'a 'o)
                  (list 'o)))
(test (c3-merge ex5 empty) (some (list 'd 'c 'b 'a 'o)))

;; get-field-from-obj: looks for a field of an object using the class __mro__
;; skip up to thisclass in __mro__, if defined.
;; optional address field added to support self aliasing in bound methods calls.
(define (get-field-from-obj [fld : symbol] 
                            [obj : CVal]
                            [w_obj : (optionof Address)]
                            [thisclass : (optionof CVal)]
                            [env : Env] 
                            [sto : Store]
                            [stk : Stack]) : Result
  (begin ;(display "GET-OBJ: ") (display fld) (display " ") (display obj) 
         ;(display " ") (display w_obj) (display "\n")
  (cond
    ;; for method objects, __call__ attribute is the object itself
    [(and (equal? (VObject-antecedent obj) 'method) (equal? fld '__call__))
     (v*s*e obj sto env)]
    ;; special lookup handling for initialized super object
    [(and (equal? (VObject-antecedent obj) 'super) (some? (hash-ref (VObject-dict obj) '__self__)))
     (local ([define w_self (hash-ref (VObject-dict obj) '__self__)]
             [define self (fetch (some-v w_self) sto)]
             [define thisclass (fetch (some-v (hash-ref (VObject-dict obj) '__thisclass__)) sto)])
       (cond
         [(and (VObject? self) (equal? (VObject-antecedent self) 'type))
          ;; obj.self is a class
          (get-field-from-cls fld self w_self (some thisclass) env sto stk)]
         [else
          ;; obj.self is an instance
          (get-field-from-obj fld self w_self (some thisclass) env sto stk)]))]
    ;; normal instance lookup
    [else
     (local ([define obj-cls (get-class obj env sto)])
       (type-case (optionof Address) (lookup-mro (get-mro obj-cls thisclass sto) fld)
         [some (w) (let ([value (fetch w sto)])
                     (cond
                       ;; For functions, create method object bound to the object itself
                       [(VClosure? value) 
                        (local [(define-values (meth sto-m) 
                                  (mk-method w obj w_obj sto))]
                          (v*s*e meth sto-m env))]
                       ;; for classmethod objects create method object bound to the object's class
                       [(and (VObject? value) 
                             (equal? (VObject-antecedent value) 'classmethod))
                        (local [(define w_func 
                                  (some-v (hash-ref (VObject-dict value) '__func__)))
                                (define-values (meth sto-m) 
                                  (mk-method w_func obj-cls (none) sto))]
                          (v*s*e meth sto-m env))]
                       ;; for staticmethod obj. return func attribute
                       [(and (VObject? value) 
                             (equal? (VObject-antecedent value) 'staticmethod))
                        (local [(define func 
                                  (v*s*e-v (get-field '__func__ value (none) env sto stk)))]
                          (v*s*e func sto env))]
                       ;; otherwise return the value of the attribute
                       [else 
                        (v*s*e value sto env)]))]
         [none () (mk-exception 'AttributeError
                                (string-append 
                                 (string-append "object " 
                                                (symbol->string (VObject-antecedent obj)))
                                 (string-append " has no attribute "
                                                (symbol->string fld)))
                                env sto stk)]))])))

;; get-field-from-cls: looks for a field of a class using class __mro__
;; skip up to thisclass in __mro__, if defined.
;; optional address field added to support self aliasing in bound methods calls.
(define (get-field-from-cls [fld : symbol] 
                            [cls : CVal]
                            [w_cls : (optionof Address)]
                            [thisclass : (optionof CVal)]
                            [env : Env] 
                            [sto : Store]
                            [stk : Stack]) : Result
  (cond 
    [(equal? fld '__mro__) 
     ;; temporary hack to avoid self-reference in __mro__
<<<<<<< HEAD
     (v*s (VObject 'tuple (some (MetaTuple (get-mro c s))) (hash empty)) s (none))]
    [else
     (type-case (optionof Address) (lookup-mro (get-mro c s) n)
       [some (w) (v*s (fetch w s) s (some w))]
       [none () (mk-exception 'AttributeError
                              (string-append 
                               (string-append
                                "object"
                                " has no attribute '")
                               (string-append
                                (symbol->string n) "'"))
                              s)])]))
=======
     (v*s*e (VObject 'tuple (some (MetaTuple (get-mro cls thisclass sto)))
                     (hash empty))
            sto env)]
    [else
     (type-case (optionof Address) (lookup-mro (get-mro cls thisclass sto) fld)
       [some (w) (let ([value (fetch w sto)])
                   (cond
                     ;; for classmethod obj. create method obj. bound to the class
                     [(and (VObject? value) 
                           (equal? (VObject-antecedent value) 'classmethod))
                      (local [(define w_func
                                (some-v (hash-ref (VObject-dict value) '__func__)))
                              (define-values (meth sto-m) 
                                (mk-method w_func cls w_cls sto))]
                        (v*s*e meth sto-m env))]
                     ;; for staticmethod obj. return func attribute
                     [(and (VObject? value) 
                           (equal? (VObject-antecedent value) 'staticmethod))
                      (local [(define func 
                                (v*s*e-v (get-field '__func__ value (none) env sto stk)))]
                        (v*s*e func sto env))]
                     ;; otherwise return the value of the attribute
                     [else 
                      (v*s*e value sto env)]))]
       [none () (mk-exception 'AttributeError
                              (string-append 
                               (string-append "class " 
                                              (symbol->string (VObject-antecedent cls)))
                               (string-append " has no attribute "
                                              (symbol->string fld)))
                              env sto stk)])]))
>>>>>>> master

;; lookup-mro: looks for field in mro list
(define (lookup-mro [mro : (listof CVal)] [n : symbol]) : (optionof Address)
  (cond
    [(empty? mro) (none)]
    [else (type-case CVal (first mro)
            [VObjectClass (antecedent mval d class)
                     (type-case (optionof Address) (hash-ref d n)
                       [none () (lookup-mro (rest mro) n)]
                       [some (value) (some value)])]
            [else (error 'lookup-mro "an entry in __mro__ list is not an object")])]))

<<<<<<< HEAD
;; immutable-type?: decide if the value of x is immutable type--str, number and tuple
(define (immutable-type? [x : CVal]) : boolean
  (type-case CVal x
    [VObjectClass (ante mval dict class)
             (if (some? mval) ;decide if the it is num, str, tuple
                 (let ((metav (some-v mval)))
                   (if (or (MetaNum? metav)
                           (MetaStr? metav)
                           (MetaTuple? metav))
                       true
                       false))
                 false)]
    [VPointer (a) false] ;VPointer will only point to mutable type
    [else true]))
=======
;; special methods
(define (is-special-method? [n : symbol])
  (member n (list '__eq__ '__cmp__ '__str__ '__getitem__ '__gt__ '__lt__ '__lte__ '__gte__)))
>>>>>>> master
