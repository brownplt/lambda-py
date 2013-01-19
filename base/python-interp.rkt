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
         (typed-in racket/list (remove-duplicates : ((listof 'a) -> (listof 'a))))
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
                         [v*s (vfr sfr) (cons first-r (rec-cascade (rest exprs) e sfr))]
                         [Return (vfr sfr) (list (return-exception sfr))]
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
                          [Continue (sb) (continue-exception sb)] 
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
                                   [Continue (sb) (continue-exception sb)] 
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
                                         [Continue (sb) (continue-exception sb)] 
                                         [Exception (vb sb) (Exception vb sb)]))))]
                             [else (error 'interp "Not a closure or constructor.")])]
                      [Return (vfun sfun) (return-exception sfun)]
                      [Break (sfun) (break-exception sfun)]
                      [Continue (sfun) (continue-exception sfun)] 
                      [Exception (vfun sfun) (Exception vfun sfun)])))]
     [else (error 'interp "Not a closure or constructor")])]
   [Return (vfun sfun) (return-exception sfun)]
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
                [(Break? body-r) (v*s vnone (Break-s body-r))]
                ;; if it resulted in a value or continue, attempt to run the loop again
                [else (interp-while test body orelse env (v*s-s body-r))]))
            (interp-env orelse env (v*s-s test-r))))))

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
              [Continue (sbody) (Continue sbody)] 
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
  (local [(define loc (new-loc))
          (define-values (val sto) (type-case Result value
                                     [v*s (v s) (values v s)]
                                     [Return (v s) (values v s)]
                                     [Break (s) (values vnone s)]
                                     [Continue (s) (values vnone s)] 
                                     [Exception (v s) (values v s)]))]
    (interp-env body
                ; Needed still?
                ; From Anand:
                #|(if (global-scope? env)
                    ; Creating a new localscope for handling CLet in global scope.
                    ; Assuming that there won't be any local bindings in the let body.
                    (cons (hash-set (hash (list)) name loc) env) 
                    (cons (hash-set (first env) name loc) (rest env)))|#
                (cons (hash-set (first env) name loc) (rest env))
                (hash-set sto loc val))))

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
                  [Continue (s) (continue-exception s)] 
                  [Exception (v s) (Exception v s)]))]
    
    [CStr (s) (v*s (VObject 'str (some (MetaStr s)) (hash empty)) sto)]
    [CTrue () (v*s true-val sto)]
    [CFalse () (v*s false-val sto)]
    [CNone () (v*s vnone sto)]
    [CUndefined () (v*s (VUndefined) sto)]

    [CClass (name bases body)
            ;; the tuple of bases is evaluated assuming global scope for class names,
            ;; should be changed for a more general lookup with the new scope implementation
            (begin (display "BEGIN CLASS\n") (display bases)
            (type-case Result (interp-env (CTuple (map (lambda (id) (CId id (GlobalId)))
                                                       bases))
                                          env sto)
              [v*s (vbases sbases)
                   (type-case Result (interp-env body (cons (hash empty) env) sto)
                     [v*s (vbody sbody)
                          (begin (display name) (display "\n")
                                 (display env) (display "\n")
                                 (display sbody) (display "\n")
                                 (let ([res (mk-type name vbases (hash empty) sbody env)])
                                   res))]
                     ;; THIS NEEDS MAJOR WORK NOW
                     #|(local [(define class-val (lookup name env))
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
                              (hash-set sbody w (make-under-dict (hash empty) sbody))))]|#
                   [Return (vval sval) (return-exception sval)]
                   [Break (sval) (break-exception sval)]
                   [Continue (sval) (continue-exception sval)] 
                   [Exception (vval sval) (Exception vval sval)])]
            [Return (vbases sbases) (return-exception sbases)]
            [Break (sbases) (break-exception sbases)]
            [Continue (sbases) (continue-exception sbases)] 
            [Exception (vbases sbases) (Exception vbases sbases)]))]
   
    [CGetField (value attr)
	       (type-case Result (interp-env value env sto)
                    [v*s (vval sval) (get-field attr vval env sval)]
                    [Return (vval sval) (return-exception sval)]
                    [Break (sval) (break-exception sval)]
                    [Continue (sval) (continue-exception sval)] 
                    [Exception (vval sval) (Exception vval sval)])]
			
    [CSeq (e1 e2) (type-case Result (interp-env e1 env sto)
                    [v*s (v1 s1) (interp-env e2 env s1)]
                    [Return (v1 s1) (Return v1 s1)]
                    [Break (s1) (Break s1)]
                    [Continue (s1) (Continue s1)] 
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
                              (hash empty))
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
                                         (hash empty))
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
                                       (hash empty))
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
                                     (hash empty))
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
                     [Continue (sv) (continue-exception sv)] 
                     [Exception (vv sv) (Exception vv sv)])]
    
    ;; is this used anymore?
    [CError (e) (type-case Result (interp-env e env sto)
                  [v*s (ve se) (raise-user-error (pretty ve))]
                  [Return (ve se) (return-exception se)]
                  [Break (se) (break-exception se)]
                  [Continue (se) (continue-exception se)] 
                  [Exception (ve se) (Exception ve se)])]

    [CIf (i t e) (type-case Result (interp-env i env sto)
                   [v*s (vi si) (if (truthy? vi)
                                    (interp-env t env si)
                                    (interp-env e env si))]
                   [Return (vi si) (return-exception si)]
                   [Break (si) (break-exception si)]
                   [Continue (si) (continue-exception si)] 
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
          (begin (display "LET: ") (display x) (display type) (display "\n")
          (interp-let x type (interp-env bind env sto) body env))]

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
               [Continue (sv) (continue-exception sv)] 
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
              [Continue (sarg) (continue-exception sarg)] 
              [Exception (varg sarg) (Exception varg sarg)])]

    [CWhile (body test orelse) (interp-while body test orelse env sto)]
    
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
                  [Continue (sexpr) (continue-exception sexpr)] 
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
                                [Continue (sfin) (Continue sfin)] 
                                [Exception (vfin sfin)
                                           (Exception vfin sfin)])]
                      [Return (velse selse) (return-exception selse)]
                      [Break (selse) (Break selse)]
                      [Continue (selse) (Continue selse)]
                      [Exception (velse selse)
                                 (Exception velse selse)])]
            [Return (vtry stry) (Return vtry stry)]
            [Break (stry) (Break stry)]
            [Continue (stry) (Continue stry)]
            ;; handle excepts here
            [Exception (vtry stry)
               (local [(define result 
                         (if (empty? excepts)
                             (Exception vtry stry)
                             (interp-excepts excepts stry env
                                             (Exception vtry stry))))]
                 ;; TODO: make sure this is still right!
                 (if (not (v*s? result))
                     (begin
                       (interp-env finally env (Exception-s result))
                       result)
                     (interp-env finally env (v*s-s result))))])]

    [CExcept (types name body) (interp-env body env sto)]
    
    [CBreak () (Break sto)]
    [CContinue () (Continue sto)]
    )))

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

(define (global-scope? [env : Env]) : boolean
  (= (length env) 1))

;; handles lookup chain for function calls on objects
;; multiple inheritance modification : for class lookup call get-field-from-class
(define (get-field [n : symbol] [c : CVal] [e : Env] [s : Store]) : Result
  (begin ;(display "GET: ") (display n) (display " ") (display c) (display "\n")
    ;(display e) (display "\n\n")
    (cond 
      [(and (some? (VObject-mval c)) (MetaClass? (some-v (VObject-mval c))))
       ;; class lookup
       (get-field-from-class n c e s)]
      [else
        ;; instance lookup
        (type-case CVal c
          [VObject (antecedent mval d) 
                   (let ([w (hash-ref (VObject-dict c) n)])
                     (begin ;(display "loc: ") (display w) (display "\n\n")
                       (type-case (optionof Address) w
                         [some (w) 
                               (v*s (fetch w s) s)]
                         [none () 
                               (local [(define __class__w (hash-ref (VObject-dict c) '__class__))]
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
  (begin (display o) (display "---") (display f) (display "\n")
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
                                    (begin (display vo) (display "\n")
                                    (hash-set so (some-v nw) 
                                              (VObject ante-name
                                                       mval
                                                       (hash-set (VObject-dict vo) f w)))))]
                              (v*s vnone
                                   (hash-set snew w v)))]))]
           [else (error 'interp "Can't assign to nonobject.")])]
    [Return (vo so) (return-exception so)]
    [Break (so) (break-exception so)]
    [Continue (so) (continue-exception so)] 
    [Exception (vo so) (Exception vo so)])))

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

(define (continue-exception [sto : Store]) : Result
  (mk-exception 'SyntaxError "'continue' outside loop" sto))

(define (interp expr)
  (type-case Result (interp-env expr (list (#|make-|#hash (list))) (hash (list)))
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
    [Continue (sexpr)
           (local [(define exn (continue-exception sexpr))]
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
             [Continue (sarg2) (continue-exception sarg2)] 
             [Exception (varg2 sarg2) (Exception varg2 sarg2)])]
      [Return (varg1 sarg1) (return-exception sarg1)]
      [Break (sarg1) (break-exception sarg1)]
      [Continue (sarg1) (continue-exception sarg1)] 
      [Exception (varg1 sarg1) (Exception varg1 sarg1)]))

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
                 [env : Env]) : Result
  (local [(define bases-list (MetaTuple-v (some-v (VObject-mval bases))))
          (define w (new-loc))
          (define bases_w (new-loc))
          (define mro_w (new-loc))]
    (type-case Result (build-mro name bases-list env sto)
      [v*s (vmro smro) 
             (v*s (VObject 'type
                           (some (MetaClass name)) 
                           (hash-set 
                             (hash-set 
                               (hash-set h-dict '__dict__ w)
                               '__bases__ bases_w)
                             '__mro__ mro_w))
                  (hash-set 
                    (hash-set 
                      (hash-set sto w (make-under-dict h-dict sto))
                      bases_w bases)
                    mro_w vmro))]
      [Return (vmro smro) (return-exception smro)]
      [Break (smro) (break-exception smro)]
      [Continue (smro) (continue-exception smro)] 
      [Exception (vmro smro) (Exception vmro smro)])))

;; build-mro: merge the __mro__ of the bases using the C3 algorithm
;; Raises TypeError if there are duplicated bases or linearization is not possible.
;; The class should be the first element of __mro__, but since this seems hard
;; to implement with immutable hashes, it will be prepended on retrieval
(define (build-mro [name : symbol] 
                   [bases : (listof CVal)] 
                   [env : Env] 
                   [sto : Store]) : Result
  ;; The mro is the c3-merge of the mro of the bases plus the list of bases
  (begin (display "BASES for ") (display name) (display ": ") (display bases) (display "\n")
  (let ([maybe-mro (c3-merge (append (map (lambda (base) (get-mro base sto)) 
                                          bases)
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
         (v*s (VObject 'tuple (some (MetaTuple (some-v maybe-mro))) (hash empty))
                sto))]))))
 
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

;; get-field-from-class: looks for a field in the class __mro__ components
(define (get-field-from-class [n : symbol] 
                              [c : CVal] 
                              [e : Env] 
                              [s : Store]) : Result
  (cond 
    [(equal? n '__mro__) 
     ;; temporary hack to avoid self-reference in __mro__
     (v*s (VObject 'tuple (some (MetaTuple (get-mro c s))) (hash empty)) s)]
    [else
     (type-case (optionof Address) (lookup-mro (get-mro c s) n)
       [some (w) (v*s (fetch w s) s)]
       [none () (mk-exception 'AttributeError
                              (string-append 
                               (string-append
                                "object"
                                " has no attribute '")
                               (string-append
                                (symbol->string n) "'"))
                              s)])]))

;; lookup-mro: looks for field in mro list
(define (lookup-mro [mro : (listof CVal)] [n : symbol]) : (optionof Address)
  (cond
    [(empty? mro) (none)]
    [else (type-case CVal (first mro)
            [VObject (antecedent mval d)
                     (type-case (optionof Address) (hash-ref d n)
                       [none () (lookup-mro (rest mro) n)]
                       [some (value) (some value)])]
            [else (error 'lookup-mro "an entry in __mro__ list is not an object")])]))
