#lang plai-typed

(require "python-core-syntax.rkt"
         "python-primitives.rkt"
         "builtins/object.rkt"
         "builtins/bool.rkt"
         "builtins/tuple.rkt"
         "builtins/num.rkt"
         "builtins/none.rkt"
         "builtins/dict.rkt"
         "util.rkt"
         (typed-in racket/base (hash-count : ((hashof 'a 'b) -> number)))
         (typed-in racket/string (string-join : ((listof string) string -> string)))
         (typed-in racket/base (raise-user-error : (string -> 'a)))
         (typed-in racket/base (hash->list : ((hashof 'a 'b)  -> (listof 'c))))
         (typed-in racket/base (car : (('a * 'b) -> 'a)))
         (typed-in racket/base (cdr : (('a * 'b) -> 'b)))
         (typed-in racket/list (last : ((listof 'a) -> 'a)))
         (typed-in racket/base (append : ((listof 'a) (listof 'a) -> (listof'a))))
         )

(define (append3 a b c)
  (append a (append b c)))

;; interp-cascade, interprets a list of expressions with an initial store,
;; environment and produces the list of results and the final store
(define (interp-cascade [exprs : (listof CExpr)] 
                        [init-s : Store]
                        [env : Env]) : ((listof Result) * Store)
  (local [(define (rec-cascade exprs e s)
            (cond [(empty? exprs) empty]
                  [(cons? exprs)
                     (let ([first-r (interp-env (first exprs) e s)])
                       (type-case Result first-r
                         [v*s (vfr sfr)
                              (cons first-r (rec-cascade (rest exprs) e sfr))]
                         [Return (vfr sfr) (list (return-exception sfr))]
                         [Break (sfr) (list first-r)]
                         [Exception (vfr sfr) (list first-r)]))]))
          (define result-list (rec-cascade exprs env init-s))
          (define ex-results (filter Exception? result-list))
          (define break-results (filter Break? result-list))]
      (if (< 0 (length ex-results))
          (values ex-results (Exception-s (first ex-results)))
          (if (< 0 (length break-results))
              (values break-results (Break-s (first break-results)))
              (values result-list
                      (if (cons? result-list)
                          (v*s-s (first (reverse result-list)))
                          init-s))))))

;; interp-capp, interprets an application of a function. It is passed the function,
;; the list of argument expressions, an optionof indicating the presence of a stararg,
;; and the env, and the store. Returns the result of interpreting the application.
(define (interp-capp [fun : CExpr] [arges : (listof CExpr)] 
                     [stararg : (optionof CExpr)] [env : Env] [sto : Store]) : Result
 (type-case Result (interp-env fun env sto)
   [v*s (vfun sfun) 
    (type-case CVal vfun
      ;; fun is a closure, bind args and apply it
      [VClosure (cenv argxs vararg body)
                (local [(define-values (argvs-r sc) (interp-cascade arges sfun env))
                        (define exn? (filter Exception? argvs-r))]
                  (if (< 0 (length exn?))
                      (first exn?)
                      (local [(define argvs (map v*s-v argvs-r))
                              (define result
                                (if (some? stararg)
                                    (local [(define sarg-r
                                              (interp-env (some-v stararg) env sc))
                                            ;; todo: support other types
                                            ;; for star args
                                            (define l (MetaTuple-v 
                                                        (some-v 
                                                          (VObject-mval 
                                                            (v*s-v sarg-r)))))
                                            (define le (map (lambda (x)
                                                              (make-builtin-num 0))
                                                            l))]
                                      (bind-and-execute body argxs vararg 
                                                        (append argvs l)
                                                        (append arges le)
                                                        env cenv
                                                        (v*s-s sarg-r))) 
                                    (bind-and-execute body argxs vararg
                                                      argvs arges env
                                                      cenv sc)))]
                        (type-case Result result
                          [v*s (vb sb) (v*s vnone sb)]
                          [Return (vb sb) (v*s vb sb)]
                          [Break (sb) (break-exception sb)]
                          [Exception (vb sb) (Exception vb sb)]))))]
      [VObject (b mval d)
               (if (and (some? mval) (MetaClass? (some-v mval)))
                  ;; This means we're calling a class, like C().
                  ;; So we want to apply its __init__ method.
                  (local [(define f (v*s-v (get-field '__init__ vfun env sfun)))
                          ;; Create an empty object. This will be the new instance of the class.
                          (define o (new-object (MetaClass-c (some-v mval)) env sfun))]
                    (type-case CVal f
                      [VClosure (cenv argxs vararg body)
                                ;; Interpret the arguments to __init__.
                         (local [(define-values (argvs-r sc) (interp-cascade arges sfun env))
                                 (define exn? (filter Exception? argvs-r))]
                           (if (< 0 (length exn?))
                               (first exn?)
                               (local [(define argvs (map v*s-v argvs-r))
                                       ;; Bind the interpreted arguments to __init__
                                       ;; and perform the application.
                                       ;; Global scope might have changed since the __init__
                                       ;; closure was created, so we replace the global scope
                                       ;; level of __init__'s environment if necessary.
                                       (define result 
                                         (bind-and-execute body argxs vararg
                                                           (cons o argvs)
                                                           (cons (CId 'init (LocalId)) arges)
                                                           env cenv sc))]
                                 (type-case Result result
                                   [v*s (vb sb) 
                                        ;; This is a bit hacky. We just created an object
                                        ;; and passed it to __init__ which may have mutated
                                        ;; it, so we need to get the original object from
                                        ;; it's first location in the store.
                                        ;; TODO is this needed anymore? I don't think so.
                                        (v*s (fetch (some-v (lookup (first argxs) env)) sb) sb)]
                                   [Return (vb sb) (v*s vb sb)]
                                   [Break (sb) (break-exception sb)]
                                   [Exception (vb sb) (Exception vb sb)]))))]
                      [else (error 'interp 
                                   "__init__ not found. THIS SHOULDN'T HAPPEN.")]))
                  ;; This means we're calling an object,
                  ;; so we want to apply its __call__ method.
                  (local [(define __call__ (get-field '__call__ vfun env sfun))]
                    (type-case Result __call__
                      [v*s (vc sc)
                           (type-case CVal vc
                             [VClosure (cenv argxs vararg body)
                                       ;; Interpret the arguments to __call__.
                               (local [(define-values (argvs-r sc)
                                           (interp-cascade arges sfun env))
                                       (define exn? (filter Exception? argvs-r))]
                                 (if (< 0 (length exn?))
                                     (first exn?)
                                     (local [(define argvs (map v*s-v argvs-r))
                                             ;; Bind the interpreted arguments to __call__
                                             ;; and perform the application.
                                             (define result 
                                               (bind-and-execute body argxs vararg
                                                                 (cons vfun argvs)
                                                                 (cons (make-builtin-num 0) 
                                                                       arges)
                                                                 env
                                                                 cenv
                                                                 sc))]
                                       (type-case Result result
                                         [v*s (vb sb) (v*s vb sb)]
                                         [Return (vb sb) (v*s vb sb)]
                                         [Break (sb) (break-exception sb)]
                                         [Exception (vb sb) (Exception vb sb)]))))]
                             [else (error 'interp "Not a closure or constructor.")])]
                      [Return (vfun sfun) (return-exception sfun)]
                      [Break (sfun) (break-exception sfun)]
                      [Exception (vfun sfun) (Exception vfun sfun)])))]
      [else (error 'interp "Not a closure or constructor")])]
   [Return (vfun sfun) (return-exception sfun)]
   [Break (sfun) (break-exception sfun)]
   [Exception (vfun sfun) (Exception vfun sfun)]))

(define (interp-while [test : CExpr] [body : CExpr] [env : Env] [sto : Store])
  (local [(define test-r (interp-env test env sto))]
    (if (or (or (Exception? test-r) (Return? test-r)) (Break? test-r))
        test-r
        (if (truthy? (v*s-v test-r))
            (local [(define body-r (interp-env body env (v*s-s test-r)))]
              (if (or (Exception? body-r) (Return? body-r))
                  body-r
                  (if (Break? body-r)
                      (v*s vnone (Break-s body-r))
                      (interp-while test body env (v*s-s body-r)))))
            (v*s vnone (v*s-s test-r))))))

;; bind-and-execute, binds the arguments in the closure's
;; environment and then applies the closure.
(define (bind-and-execute [body : CExpr] [argxs : (listof symbol)]
                          [vararg : (optionof symbol)] [argvs : (listof CVal)]
                          [arges : (listof CExpr)] [env : Env]
                          [ext : Env] [sto : Store]) : Result
  (local [(define-values (e s mayb-ex) 
            (bind-args argxs vararg argvs arges env ext sto))]
    (if (some? mayb-ex)
        (some-v mayb-ex)
        (interp-env body e s))))

(define (interp-excepts [excepts : (listof CExpr)]
                        [sto : Store]
                        [env : Env]
                        [exn : Result]) : Result
  (local [(define exn-type (VObject-antecedent (Exception-v exn)))
          (define (find-match type exps fsto)
            (cond
              [(empty? exps) (values (none) fsto (none))]
              [(cons? exps)
               ;; need to interp exprs and then check
               (local [(define-values (except-types-results tsto)
                         (interp-cascade (CExcept-types (first exps)) fsto env))
                       (define exn? (filter Exception? except-types-results))]
                 (if (< 0 (length exn?))
                     (values (none) tsto (some (first exn?)))
                     (local [(define actual-except-types
                               (if (and (> (length except-types-results) 0)
                                        (VObject? (v*s-v (first except-types-results)))
                                        (MetaTuple?
                                          (some-v (VObject-mval
                                                    (v*s-v (first except-types-results))))))
                                   (map (λ (v) (v*s v
                                                    (v*s-s (first except-types-results))))
                                        (MetaTuple-v
                                          (some-v (VObject-mval
                                                    (v*s-v (first except-types-results))))))
                                   except-types-results))
                             (define except-types
                                 (map (lambda (t)
                                         (type-case CVal (v*s-v t)
                                           [VObject (ante mval dict) 
                                                    (if (and (some? mval) 
                                                             (MetaClass? (some-v mval)))
                                                      (some (MetaClass-c (some-v mval)))
                                                      (none))]
                                           [else (none)]))
                                       actual-except-types))
                                (define exn-again? (filter none? except-types))]
                                (if (< 0 (length exn-again?))
                                  (values (none) tsto
                                          (some (mk-exception
                                                  'TypeError
                                                  "can't catch closures. This will go away."
                                                  tsto)))
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
                                ;; This could potentially be in global scope. Does it matter?
                                (LocalId)
                                (Exception (Exception-v exn) hsto)
                                (CExcept-body (some-v match?))
                                env)
                    (interp-env (some-v match?) env hsto))])
            (type-case Result result
              [v*s (vbody sbody) (v*s vnone sbody)]
              [Return (vbody sbody) (return-exception sbody)]
              [Break (sbody) (Break sbody)]
              [Exception (vbody sbody)
                         (local [(define exn-args (hash-ref (VObject-dict vbody) 'args))
                                 (define exn-args-val (if (some? exn-args)
                                                          (some (fetch (some-v exn-args) sbody))
                                                          (none)))
                                 (define exn-mval (if (some? exn-args-val)
                                                      (VObject-mval (some-v exn-args-val))
                                                      (none)))
                                 (define exn-tuple (if (some? exn-mval)
                                                       (some (MetaTuple-v (some-v exn-mval)))
                                                       (none)))
                                 (define exn-tuple-mval (if (and (some? exn-tuple)
                                                                 (> (length (some-v exn-tuple))
                                                                    0))
                                                            (VObject-mval
                                                              (first (some-v exn-tuple)))
                                                            (none)))
                                 (define exn-str (if (some? exn-tuple-mval)
                                                     (MetaStr-s (some-v exn-tuple-mval))
                                                     ""))]
                         (if (string=? exn-str "No active exception to reraise")
                             (Exception (Exception-v exn) sbody)
                             result))])))
        (Exception (Exception-v exn) hsto)))))

(define (interp-let [name : symbol] [type : IdType] [value : Result]
                    [body : CExpr] [env : Env]) : Result
  (let ([loc (new-loc)])
    (type-case Result value
      [v*s (vb sb) (interp-env body
                               (cons (hash-set (first env) name loc) (rest env))
                               (hash-set sb loc vb))]
      [Return (vb sb) (interp-env body
                                  (cons (hash-set (first env) name loc) (rest env))
                                  (hash-set sb loc vb))]
      [Break (sb) (interp-env body
                              (cons (hash-set (first env) name loc) (rest env))
                              (hash-set sb loc vnone))]
      [Exception (vb sb) (interp-env body
                                     (cons (hash-set (first env) name loc) (rest env))
                                     (hash-set sb loc vb))])))

;; interp-env : CExpr * Env * Store -> Result
(define (interp-env [expr : CExpr] [env : Env] [sto : Store]) : Result
  (begin ;(display expr) (display "\n")
  (type-case CExpr expr
    [CModule (prelude body)
             (local [(define prelude-r (interp-env prelude env sto))]
                (type-case Result prelude-r
                  [v*s (v s) (interp-env body env s)]
                  [Return (v s) (return-exception s)]
                  [Break (s) (break-exception s)]
                  [Exception (v s) (Exception v s)]))]
    
    [CStr (s) (v*s (VObject 'str (some (MetaStr s)) (hash empty)) sto)]
    [CTrue () (v*s true-val sto)]
    [CFalse () (v*s false-val sto)]
    [CNone () (v*s vnone sto)]
    [CUndefined () (v*s (VUndefined) sto)]

    [CClass (name base body) 
            (type-case Result (interp-env body (cons (hash empty) env) sto)
              [v*s (vbody sbody)
                   (local [(define class-val (lookup name env))
                           (define w (if (some? class-val)
                                         (some-v class-val)
                                         (new-loc)))]
                     (v*s (VObject base 
                                   (some (MetaClass name)) 
                                   ;; THIS IS VERY CLEARLY WRONG NOW BECAUSE WE CANNOT PULL
                                   ;; NEWLY CREATED ENVINRONMENT OUT OF THE RESULT.
                                   ;; THUS, WE NEED TO ALTER DESUGAR TO HAVE THE BODY OF A
                                   ;; CLASS PRODUCE A MetaSimpleDict WHICH WILL BE PLACED HERE.
                                   (hash-set (hash empty) '__dict__ w))
                          (hash-set sbody w (make-under-dict (hash empty) sbody))))]
              [Return (vval sval) (return-exception sval)]
              [Break (sval) (break-exception sval)]
              [Exception (vval sval) (Exception vval sval)])]
    
    [CGetField (value attr)
	       (type-case Result (interp-env value env sto)
                    [v*s (vval sval) (get-field attr vval env sval)]
                    [Return (vval sval) (return-exception sval)]
                    [Break (sval) (break-exception sval)]
                    [Exception (vval sval) (Exception vval sval)])]
			
    [CSeq (e1 e2) (type-case Result (interp-env e1 env sto)
                    [v*s (v1 s1) (interp-env e2 env s1)]
                    [Return (v1 s1) (Return v1 s1)]
                    [Break (s1) (Break s1)]
                    [Exception (v1 s1) (Exception v1 s1)])]
    
    ;; note that for now we're assuming that dict keys and values aren't going
    ;; to mess with the environment and store, but this might be wrong
    [CDict (contents)
           (letrec ([interped-hash (make-hash empty)]
                    [interp-pairs (lambda (lst)
                                  (map (lambda (pair)
                                       (hash-set! interped-hash
                                                   (v*s-v (interp-env (car pair) env sto))
                                                   (v*s-v (interp-env (cdr pair) env sto))))
                                      lst))])
             (begin
               (interp-pairs (hash->list contents))
               (v*s (VObject '$dict
                              (some (MetaDict interped-hash))
                              (make-hash empty))
                    sto)))]

    [CSet (elts)
          (local [(define-values (result-list new-s)
                                     (interp-cascade elts sto env))]
              (let ([exn? (filter Exception? result-list)])
                  (if (< 0 (length exn?))
                      (first exn?) 
                      (let ([val-list (map v*s-v result-list)])
                           (v*s (VObject 'set
                                         (some (MetaSet (make-set val-list)))
                                         (make-hash empty))
                                new-s)))))]
    
    [CList (values)
           (local [(define-values (result-list new-s)
                                      (interp-cascade values sto env))]
               (let ([exn? (filter Exception? result-list)])
                   (if (< 0 (length exn?))
                       (first exn?)
                       (let ([val-list (map v*s-v result-list)])
                         (v*s (VObject 'list
                                       (some (MetaList val-list))
                                       (make-hash empty))
                              new-s)))))]

    [CTuple (values)
           (local [(define-values (result-list new-s)
                                      (interp-cascade values sto env))]
               (let ([exn? (filter Exception? result-list)])
                   (if (< 0 (length exn?))
                       (first exn?) 
                      (let ([val-list (map v*s-v result-list)])
                       (v*s (VObject 'tuple
                                     (some (MetaTuple val-list))
                                     (make-hash empty))
                            new-s)))))]

    ;; only for ids!
    [CAssign (t v) (type-case Result (interp-env v env sto)
                     [v*s (vv sv)
                          (type-case CExpr t
                            [CId (x type) (assign-to-id t vv env sv)]
                            [CGetField (o a) (assign-to-field o a vv env sv)]
                            [else (mk-exception 'SyntaxError
                                                "can't assign to literals"
                                                sv)])]
                     [Return (vv sv) (return-exception sv)]
                     [Break (sv) (break-exception sv)]
                     [Exception (vv sv) (Exception vv sv)])]
    
    ;; is this used anymore?
    [CError (e) (type-case Result (interp-env e env sto)
                  [v*s (ve se) (raise-user-error (pretty ve))]
                  [Return (ve se) (return-exception se)]
                  [Break (se) (break-exception se)]
                  [Exception (ve se) (Exception ve se)])]

    [CIf (i t e) (type-case Result (interp-env i env sto)
                   [v*s (vi si) (if (truthy? vi)
                                    (interp-env t env si)
                                    (interp-env e env si))]
                   [Return (vi si) (return-exception si)]
                   [Break (si) (break-exception si)]
                   [Exception (vi si) (Exception vi si)])]

    [CId (x t)
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
           (type-case IdType t
             [LocalId () 
               (local [(define local-w (lookup-local x env))]
                 (if (some? local-w)
                     (type-case CVal (fetch (some-v local-w) sto)
                       [VUndefined () (mk-exception 'UnboundLocalError
                                                  unboundlocal-error-str
                                                  sto)]
                       [else (v*s (fetch (some-v local-w) sto) sto)])
                     (local [(define full-w (lookup x env))]
                       (if (some? full-w)
                           (type-case CVal (fetch (some-v full-w) sto)
                             [VUndefined () (mk-exception 'NameError freevar-error-str sto)]
                             [else (v*s (fetch (some-v full-w) sto) sto)])
                           (mk-exception 'NameError
                                       (string-append "global " name-error-str)
                                       sto)))))]
             [GlobalId ()
               (local [(define full-w (lookup x env))]
                 (if (some? full-w)
                     (local [(define full-val (fetch (some-v full-w) sto))]
                       (type-case CVal full-val
                         [VUndefined () (mk-exception 'NameError name-error-str sto)]
                         [else (v*s full-val sto)]))
                     (mk-exception 'NameError name-error-str sto)))]))]

    [CObject (c mval) (v*s (VObject c mval (hash empty)) sto)]

    [CLet (x type bind body)
          (interp-let x type (interp-env bind env sto) body env)]

    [CApp (fun arges sarg)
          (interp-capp fun arges
                       (if (none? sarg)
                           (some (CTuple empty))
                           sarg)
                       env sto)]

    [CFunc (args sargs body method?) 
           (v*s (VClosure
                  (cons (hash empty) (if method? (rest env) env))
                  args sargs body)
                sto)]

    [CReturn (value)
             (type-case Result (interp-env value env sto)
               [v*s (vv sv) (Return vv sv)]
               [Return (vv sv) (return-exception sv)]
               [Break (sv) (break-exception sv)]
               [Exception (vv sv) (Exception vv sv)])]

    [CPrim1 (prim arg)
            (type-case Result (interp-env arg env sto)
              [v*s (varg sarg) 
                   (case prim
                     ['Not (if (truthy? varg)
                             (v*s false-val sarg)
                             (v*s true-val sarg))]
                     [else (v*s (python-prim1 prim varg) sarg)])]
              [Return (varg sarg) (return-exception sarg)]
              [Break (sarg) (break-exception sarg)]
              [Exception (varg sarg) (Exception varg sarg)])]

    [CWhile (body test orelse) (interp-while body test env sto)]
    
    ;; implement this
    [CPrim2 (prim arg1 arg2) (interp-cprim2 prim arg1 arg2 sto env)]
    
    [CBuiltinPrim (op args) (local [(define-values (result-list new-s)
                                      (interp-cascade args sto env))]
                               (let ([exn? (filter Exception? result-list)])
                                 (if (< 0 (length exn?))
                                     (first exn?)
                                     (let ([val-list (map v*s-v result-list)])
                                       (local [(define mayb-val 
                                               (builtin-prim op val-list env new-s))] 
                                              (if (some? mayb-val)
                                       (v*s (some-v mayb-val) new-s)
                                       ;; todo: more useful errors
                                       (mk-exception 'TypeError "Bad types in builtin call" 
                                                     sto)))))))]
    [CRaise (expr) 
            (if (some? expr)
                (type-case Result (interp-env (some-v expr) env sto)
                  [v*s (vexpr sexpr)
                       (cond
                         [(and (VObject? vexpr) (object-is? vexpr 'Exception env sto))
                          (Exception vexpr sexpr)]
                         [else (mk-exception 'TypeError
                                             "exceptions must derive from BaseException"
                                             sexpr)])]
                  [Return (vexpr sexpr) (return-exception sexpr)]
                  [Break (sexpr) (break-exception sexpr)]
                  [Exception (vexpr sexpr) (Exception vexpr sexpr)])
                (mk-exception 'RuntimeError
                              "No active exception to reraise"
                              sto))]
    
    [CTryExceptElseFinally (try excepts orelse finally)
         (type-case Result (interp-env try env sto)
            [v*s (vtry stry)
                   (type-case Result (interp-env orelse env stry)
                      [v*s (velse selse)
                             (type-case Result (interp-env finally env selse)
                                [v*s (vfin sfin)
                                       (v*s vnone sfin)]
                                [Return (vfin sfin) (return-exception sfin)]
                                [Break (sfin) (Break sfin)]
                                [Exception (vfin sfin)
                                           (Exception vfin sfin)])]
                      [Return (velse selse) (return-exception selse)]
                      [Break (selse) (Break selse)]
                      [Exception (velse selse)
                                 (Exception velse selse)])]
            [Return (vtry stry) (Return vtry stry)]
            [Break (stry) (Break stry)]
            ;; handle excepts here
            [Exception (vtry stry)
               (local [(define result 
                         (if (empty? excepts)
                             (Exception vtry stry)
                             (interp-excepts excepts stry env
                                             (Exception vtry stry))))]
                 (if (Exception? result)
                     (begin
                       (interp-env finally env (Exception-s result))
                       result)
                     (if (Break? result)
                         result
                         (interp-env finally env (v*s-s result)))))])]

    [CExcept (types name body) (interp-env body env sto)]
    
    [CBreak () (Break sto)])))

(define (assign-to-id [id : CExpr] [v : CVal] [env : Env] [sto : Store]) : Result
  (local [(define mayb-loc (lookup (CId-x id) env))]
    (if (some? mayb-loc)
        (v*s vnone (hash-set sto (some-v mayb-loc) v))
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

;; handles lookup chain for function calls on objects
;; looks in object dict, then class dict, then base class dicts, then default class
;; order in which base class dicts are traversed depends on truth value of super
(define (get-field [n : symbol] [c : CVal] [e : Env] [s : Store]) : Result
  (begin ;(display "GET: ") (display n) (display " ") (display c) (display "\n")
         ;(display e) (display "\n\n")
  (type-case CVal c
    [VObject (antecedent mval d) 
                    (let ([w (hash-ref (VObject-dict c) n)])
              (begin ;(display "loc: ") (display w) (display "\n\n")
                (type-case (optionof Address) w
                [some (w) (v*s (fetch w s) s)]
                [none () (local [(define __class__w (hash-ref (VObject-dict c) '__class__))]
                           (type-case (optionof Address) __class__w
                             [some (w) (get-field n (fetch (some-v __class__w) s) e s)]
                             [none () (let ([mayb-base (lookup antecedent e)])
                                        (if (some? mayb-base)
                                          (let ([base (fetch (some-v mayb-base) s)])
                                            (get-field n base e s))
                                          (mk-exception 'AttributeError
                                                        (string-append 
                                                          (string-append
                                                            "object"
                                                            " has no attribute '")
                                                          (string-append
                                                            (symbol->string n) "'"))
                                                        s)))]))])))]
    [else (error 'interp "Not an object with functions.")])))

(define (assign-to-field o f v e s)
  (type-case Result (interp-env o e s)
    [v*s (vo so)
         (type-case CVal vo
           [VObject (ante-name mval d)
             (local [(define loc (hash-ref (VObject-dict vo) f))]
               (type-case (optionof Address) loc
                 [some (w) (v*s vnone (hash-set so w v))]
                 [none () (local [(define w (new-loc))
                                  (define nw (hash-ref (first e) (CId-x o)))
                                  (define snew
                                    (begin (display nw) (display "\n")
                                    (hash-set so (some-v nw) 
                                              (VObject ante-name
                                                       mval
                                                       (hash-set (VObject-dict vo) f w)))))]
                              (v*s vnone
                                   (hash-set snew w v)))]))]
           [else (error 'interp "Can't assign to nonobject.")])]
    [Return (vo so) (return-exception so)]
    [Break (so) (break-exception so)]
    [Exception (vo so) (Exception vo so)]))

(define (new-object [c-name : symbol] [e : Env] [s : Store]) : CVal
  (VObject c-name (none) (hash empty)))

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
                   [vals : (listof CVal)] 
                   [arges : (listof CExpr)] 
                   [env : Env] [ext : Env]
                   [sto : Store]) : (Env * Store * (optionof Result))
  (cond [(and (empty? args) (empty? vals))
         ;; This means we've bound all of the values to the argument symbols,
         ;; so we want to bind an empty tuple to the stararg symbol if there is one.
         (if (some? sarg)
           (bind-args (list (some-v sarg))
                      (none)
                      (list (make-builtin-tuple empty))
                      (list (make-builtin-num 0))
                      env 
                      ext
                      sto)
           (values ext sto (none)))]
        [(and (empty? args) (some? sarg)) 
         ;; This means we have a stararg symbol and extra arguments, so we'll
         ;; place them in a tuple and bind it to the stararg symbol.
         (let ([star-tuple (make-builtin-tuple vals)])
           (bind-args (list (some-v sarg))
                      (none) 
                      (list star-tuple)
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
                 (define mutability-check
                   (lambda ()
                     (type-case CExpr (first arges)
                       [CId (x t)
                            (if (symbol=? x 'init)
                                (new-loc)
                                (some-v (lookup x env)))]
                       [else (new-loc)])))
                 (define where 
                   (type-case CVal val
                     [VObject (ante-name mayb-mval dict)
                       (if (some? mayb-mval)
                         (let ([mval (some-v mayb-mval)])
                           (type-case MetaVal mval
                             [MetaClass (c) (mutability-check)]
                             [MetaList (l) (mutability-check)]
                             ;;[MetaDict (d) (;; get loc of val in store)]
                             ;; immutable types should get a new store location
                             [else (new-loc)]))
                         (mutability-check))]
                     [else (new-loc)]))
                 (define e (cons (hash-set (first ext) (first args) where) (rest ext)))
                 (define s (hash-set sto where (first vals)))]
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

(define (interp expr)
  (type-case Result (interp-env expr (list (hash (list))) (hash (list)))
    [v*s (vexpr sexpr)
           (if (not (MetaNone? (some-v (VObject-mval vexpr))))
             (begin (display (pretty vexpr)) 
                    (display "\n"))
             (display ""))]
    [Return (vexpr sexpr)
            (local [(define exn (return-exception sexpr))]
              (raise-user-error (pretty-exception (Exception-v exn)
                                                  (Exception-s exn))))]
    [Break (sexpr)
           (local [(define exn (break-exception sexpr))]
             (raise-user-error (pretty-exception (Exception-v exn)
                                                 (Exception-s exn))))]
    [Exception (vexpr sexpr)
               (raise-user-error (pretty-exception vexpr sexpr))]))

(define (truthy? [val : CVal]) : boolean
  (type-case CVal val
    [VClosure (e a s b) true]
    [VObject (a mval d) (truthy-object? (VObject a mval d))]
    [VUndefined () false]))

(define (interp-cprim2 [prim : symbol] 
                       [arg1 : CExpr]
                       [arg2 : CExpr]
                       [sto : Store]
                       [env : Env]) : Result

    (type-case Result (interp-env arg1 env sto)
      [v*s (varg1 sarg1)
           (type-case Result (interp-env arg2 env sarg1)
             [v*s (varg2 sarg2) 
                  (case prim
                    ;; Handle Is, IsNot, In, NotIn
                    ['Is (if (is? varg1 varg2)
                           (v*s true-val sarg2)
                           (v*s false-val sarg2))]
                    ['IsNot (if (not (is? varg1 varg2))
                           (v*s true-val sarg2)
                           (v*s false-val sarg2))]
                    [else (error 'interp (string-append "Haven't implemented a case yet: "
                                                        (symbol->string
                                                          prim)))])]
             [Return (varg2 sarg2) (return-exception sarg2)]
             [Break (sarg2) (break-exception sarg2)]
             [Exception (varg2 sarg2) (Exception varg2 sarg2)])]
      [Return (varg1 sarg1) (return-exception sarg1)]
      [Break (sarg1) (break-exception sarg1)]
      [Exception (varg1 sarg1) (Exception varg1 sarg1)]))
