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
         (typed-in racket/base (hash-copy : ((hashof 'a 'b) -> (hashof 'a 'b))))
         (typed-in racket/base (hash-count : ((hashof 'a 'b) -> number)))
         (typed-in racket/base (expt : (number number -> number)))
         (typed-in racket/base (quotient : (number number -> number)))
         (typed-in racket/string (string-join : ((listof string) string -> string)))
         (typed-in racket/base (string<? : (string string -> boolean)))
         (typed-in racket/base (string>? : (string string -> boolean)))
         (typed-in racket/base (string<=? : (string string -> boolean)))
         (typed-in racket/base (string>=? : (string string -> boolean)))
         (typed-in racket/base (for-each : (('a -> void) (listof number) -> 'b)))
         (typed-in racket/base (raise-user-error : (string -> 'a)))
         (typed-in racket/base (ormap : (('a -> boolean) (listof 'a) -> 'a)))
         (typed-in racket/base (hash->list : ((hashof 'a 'b)  -> (listof 'c))))
         (typed-in racket/base (car : (('a * 'b) -> 'a)))
         (typed-in racket/base (cdr : (('a * 'b) -> 'b)))
         (typed-in racket/list (last : ((listof 'a) -> 'a)))
         (typed-in racket/base (append : ((listof 'a) (listof 'a) -> (listof'a))))
         (typed-in racket/list (drop-right : ((listof 'a) number -> (listof 'a))))
         (typed-in racket/list (drop : ((listof 'a) number -> (listof 'a))))
         (typed-in racket/list (take : ((listof 'a) number -> (listof 'a))))
         )

(define (append3 a b c)
  (append a (append b c)))

;; interp-cascade, interprets a list of expressions with an initial store,
;; environment and produces the list of results and the final environment and
;; store using the values/define-values 
(define (interp-cascade [exprs : (listof CExpr)] 
                        [init-s : Store]
                        [init-e : Env]) : ((listof Result) * Store * Env)
  (local [(define (rec-cascade exprs e s)
            (cond [(empty? exprs) empty]
                  [(cons? exprs)
                     (let ([first-r (interp-env (first exprs) e s)])
                       (type-case Result first-r
                         [v*s*e (vfr sfr efr)
                                (cons first-r (rec-cascade (rest exprs) efr sfr))]
                         [Return (vfr sfr efr) (list (return-exception efr sfr))]
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

(define (replace-global-scope? [ext : Env] [curr : Env]) : Env
  (if (and (cons? curr)
           (cons? ext)
           (> (hash-count (last curr)) (hash-count (last ext))))
      (append (drop-right ext 1) (list (last curr)))
      ext))

(define (interp-capp [fun : CExpr] [arges : (listof CExpr)] 
                     [stararg : (optionof CExpr)] [env : Env] [sto : Store]) : Result
  (begin ;(display "APP: ") (display fun) (display "\n") (display arges) (display "\n\n\n")
         ;(display env) (display "\n\n")
 (type-case Result (interp-env fun env sto)
   [v*s*e (vfun sfun efun) 
    (type-case CVal vfun
      [VClosure (cenv argxs vararg body)
                  (local [(define-values (argvs-r sc ec) (interp-cascade arges sfun efun))]
                     (let ([exn? (filter Exception? argvs-r)])
                        (if (< 0 (length exn?))
                            (first exn?)
                            (local [(define argvs (map v*s*e-v argvs-r))
                                    (define result (if (some? stararg)
                                             (letrec ([sarg-r (interp-env (some-v
                                                                            stararg) ec sc)]
                                                      ;; todo: support other types
                                                      ;; for star args
                                                      [l (MetaTuple-v 
                                                           (some-v 
                                                             (VObject-mval 
                                                               (v*s*e-v
                                                                 sarg-r))))])
                                               (bind-and-execute 
                                                 body argxs vararg 
                                                 (append argvs l)
                                                 (append arges (map
                                                                 (lambda(x)
                                                                   (make-builtin-num 0))
                                                                 l))
                                                 (v*s*e-e sarg-r)
                                                 (replace-global-scope? cenv ec)
                                                 (v*s*e-s sarg-r))) 
                                             (bind-and-execute body argxs vararg
                                                               argvs arges ec
                                                               (replace-global-scope? cenv ec)
                                                               sc)))]
                              (type-case Result result
                                [v*s*e (vb sb eb) (v*s*e vnone sb
                                                         (replace-global-scope? env eb))]
                                [Return (vb sb eb) (v*s*e vb sb 
                                                          (replace-global-scope? env eb))]
                                [Break (sb eb) (break-exception 
                                                 (replace-global-scope? env eb) sb)]
                                [Exception (vb sb eb)
                                           (Exception vb sb
                                                      (replace-global-scope? env eb))])))))]
      [VObject (b mval d)
               (if (and (some? mval) (MetaClass? (some-v mval)))
                  ; We're calling a class.
                  ; Get its constructor
                  (let ([f (v*s*e-v (get-field '__init__ vfun efun sfun))]
                        ; Create an empty object. This will be the instance of that class.
                        [o (new-object (MetaClass-c (some-v mval)) efun sfun)])
                    (type-case CVal f
                      [VClosure (cenv argxs vararg body)
                                ; interpret the arguments to the constructor
                         (local [(define-values (argvs-r sc ec)
                                   (interp-cascade arges sfun efun))]
                                (let ([exn? (filter Exception? argvs-r)])
                                    (if (< 0 (length exn?))
                                        (first exn?)
                                      (local [(define argvs (map v*s*e-v argvs-r))
                                             ; bind the interpreted arguments to the constructor
                                              (define result 
                                                (bind-and-execute body argxs vararg
                                                                (cons o argvs)
                                                                (cons (CId 'init (LocalId)) 
                                                                      arges)
                                                                ec
                                                                (replace-global-scope? cenv ec)
                                                                sc))]
                                        (type-case Result result
                                          [v*s*e (vb sb eb) 
                                                 (v*s*e 
                                                   (let ([obj (fetch (some-v 
                                                                       (lookup (first argxs)
                                                                               eb))
                                                                     sb)])
                                                     obj)
                                                   sb (replace-global-scope? env eb))]
                                         [Return (vb sb eb)
                                                 (v*s*e vb sb 
                                                        (replace-global-scope? env eb))]
                                         [Break (sb eb) (break-exception 
                                                          (replace-global-scope? env eb) sb)]
                                         [Exception (vb sb eb)
                                                 (Exception vb sb 
                                                        (replace-global-scope? env eb))])))))]
                      [else (error 'interp 
                                   "__init__ not found. THIS SHOULDN'T HAPPEN.")]))
                                     
                  (local [(define __call__ (get-field '__call__ vfun efun sfun))]
                    (type-case Result __call__
                      [v*s*e (vc sc ec)
                             (type-case CVal vc
                               [VClosure (cenv argxs vararg body)
                                         ; interpret the arguments to the constructor
                                         (local [(define-values (argvs-r sc ec)
                                                   (interp-cascade arges sfun efun))]
                                                (let ([exn? (filter Exception? argvs-r)])
                                                  (if (< 0 (length exn?))
                                                      (first exn?)
                                                      (local [(define argvs
                                                                (map v*s*e-v argvs-r))
                                             ; bind the interpreted arguments to the constructor
                                                        (define result 
                                                    (bind-and-execute body argxs vararg
                                                             (cons vfun argvs)
                                                             (cons (make-builtin-num 0) 
                                                                   arges)
                                                             ec
                                                             (replace-global-scope? cenv ec)
                                                             sc))]
                                       (type-case Result result
                                         [v*s*e (vb sb eb)
                                                (v*s*e vb sb
                                                       (replace-global-scope? env eb))]
                                         [Return (vb sb eb)
                                                 (v*s*e vb sb (replace-global-scope? env eb))]
                                         [Break (sb eb)
                                                (break-exception (replace-global-scope? env eb)
                                                                 sb)]
                                         [Exception (vb sb eb)
                                                    (Exception vb sb
                                                         (replace-global-scope? env eb))])))))]
                      [else (error 'interp 
                                   "Not a closure or constructor.")])]
                      [Return (vfun sfun efun) (return-exception efun sfun)]
                      [Break (sfun efun) (break-exception efun sfun)]
                      [Exception (vfun sfun efun) (Exception vfun sfun efun)])))]
      [else (error 'interp "Not a closure or constructor")])]
   [Return (vfun sfun efun) (return-exception efun sfun)]
   [Break (sfun efun) (break-exception efun sfun)]
   [Exception (vfun sfun efun) (Exception vfun sfun efun)])))

(define (interp-while [test : CExpr] [body : CExpr] 
                      [env : Env] [sto : Store])
  (local [(define test-r (interp-env test env sto))]
    (if (or (or (Exception? test-r) (Return? test-r)) (Break? test-r))
      test-r
      (if (truthy? (v*s*e-v test-r))
        (local [(define body-r (interp-env body (v*s*e-e test-r)
                                                (v*s*e-s test-r)))]
               (if (or (Exception? body-r) (Return? body-r))
                 body-r
                 (if (Break? body-r)
                   (v*s*e
                     vnone
                     (Break-s body-r)
                     (Break-e body-r)) 
                   (interp-while test body 
                                 (v*s*e-e body-r) 
                                 (v*s*e-s body-r)))))
        (v*s*e
          vnone
          (v*s*e-s test-r)
          (v*s*e-e test-r))))))




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
          (define (find-match type exps fsto fenv)
            (cond
              [(empty? exps) (values (none)
                                     fsto
                                     fenv
                                     (none))]
              [(cons? exps)
               ;; need to interp exprs and then check
               (local [(define-values (except-types-results tsto tenv)
                         (interp-cascade (CExcept-types (first exps)) fsto fenv))
                       (define exn? (filter Exception? except-types-results))]
                 (if (< 0 (length exn?))
                     (values (none)
                             tsto
                             tenv
                             (some (first exn?)))
                     (local [(define actual-except-types
                               (if (and (> (length except-types-results) 0)
                                        (VObject? (v*s*e-v (first except-types-results)))
                                        (MetaTuple?
                                          (some-v (VObject-mval
                                                    (v*s*e-v (first except-types-results))))))
                                   (map (λ (v) (v*s*e v
                                                      (v*s*e-s (first except-types-results))
                                                      (v*s*e-e (first except-types-results))))
                                        (MetaTuple-v
                                          (some-v (VObject-mval
                                                    (v*s*e-v 
                                                      (first except-types-results))))))
                                   except-types-results))
                             (define except-types
                                 (map (lambda (t)
                                         (type-case CVal (v*s*e-v t)
                                           [VObject (ante mval dict) 
                                                    (if (and (some? mval) 
                                                             (MetaClass? (some-v mval)))
                                                      (some (MetaClass-c (some-v mval)))
                                                      (none))]
                                           [else (none)]))
                                       actual-except-types))
                                (define exn-again? (filter none? except-types))]
                                (if (< 0 (length exn-again?))
                                  (values (none)
                                          tsto
                                          tenv
                                          (some (mk-exception
                                                  'TypeError
                                                  "can't catch closures. This will go away."
                                                  tenv
                                                  tsto)))
                                  (if (or (member exn-type (map some-v except-types))
                                          (empty? (CExcept-types (first exps))))
                                    (values (some (first exps))
                                            tsto
                                            tenv
                                            (none))
                                    (find-match type (rest exps) tsto tenv))))))]))
          (define-values (match? hsto henv exn?) (find-match exn-type excepts sto env))]

    ; we might have found a matching except clause
    (if (some? exn?)
      (some-v exn?)
      (if (some? match?)
        (let ([as-name (CExcept-name (some-v match?))])
          (let ([result
                  (if (some? as-name)
                    (interp-let (some-v as-name)
                                (Exception (Exception-v exn) hsto henv)
                                (CExcept-body (some-v match?)))
                    (interp-env (some-v match?) henv hsto))])
            (type-case Result result
              [v*s*e (vbody sbody ebody) (v*s*e vnone sbody ebody)]
              [Return (vbody sbody ebody) (return-exception ebody sbody)]
              [Break (sbody ebody) (Break sbody ebody)]
              [Exception (vbody sbody ebody)
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
                             (Exception (Exception-v exn) sbody ebody)
                             result))])))
        (Exception (Exception-v exn) hsto henv)))))

(define (interp-let [name : symbol] [value : Result] [body : CExpr]) : Result
  (let ([loc (new-loc)])
    (type-case Result value
      [v*s*e (vb sb eb)
             (interp-env body
                         (cons (hash-set (first eb) name loc) (rest eb))
                         (hash-set sb loc vb))]
      [Return (vb sb eb) (interp-env body
                         (cons (hash-set (first eb) name loc) (rest eb))
                         (hash-set sb loc vb))]
      [Break (sb eb) (interp-env body
                         (cons (hash-set (first eb) name loc) (rest eb))
                         (hash-set sb loc vnone))]
      [Exception (vb sb eb)
                 (interp-env body
                         (cons (hash-set (first eb) name loc) (rest eb))
                         (hash-set sb loc vb))])))

;; interp-env : CExpr * Env * Store -> Result
(define (interp-env [expr : CExpr] [env : Env] [sto : Store]) : Result
  (type-case CExpr expr
    [CModule (prelude body)
             (local [(define prelude-r (interp-env prelude env sto))]
                (type-case Result prelude-r
                    [v*s*e (v s e) (interp-env body e s)]
                    [Return (v s e) (return-exception e s)]
                    [Break (s e) (break-exception e s)]
                    [Exception (v s e) (Exception v s e)]))]
    
    [CStr (s) (v*s*e (VObject 'str (some (MetaStr s)) (hash empty)) sto env)]
    [CTrue () (v*s*e true-val sto env)]
    [CFalse () (v*s*e false-val sto env)]
    [CNone () (v*s*e vnone sto env)]
    [CUndefined () (v*s*e (VUndefined) sto env)]

    [CClass (name base body) (begin ;(display "Class ") (display env) (display "\n")
               (type-case Result (interp-env body (cons (hash empty) env) sto)
                 [v*s*e (vbody sbody ebody)
                        (local [(define w (new-loc))]
                          (v*s*e (VObject base 
                                          (some (MetaClass name)) 
                                          (hash-set (first ebody)
                                                    '__dict__
                                                    w))
                                 (hash-set sbody w (make-under-dict (first ebody) sbody))
                                 env))]
                 [Return (vval sval eval) (return-exception eval sval)]
                 [Break (sval eval) (break-exception eval sval)]
                 [Exception (vval sval eval) (Exception vval sval eval)]))]
    
    [CGetField (value attr)
	       (type-case Result (interp-env value env sto)
                    [v*s*e (vval sval eval)
                           (get-field attr vval eval sval)]
                    [Return (vval sval eval) (return-exception eval sval)]
                    [Break (sval eval) (break-exception eval sval)]
                    [Exception (vval sval eval) (Exception vval sval eval)])]
			
    [CSeq (e1 e2) (type-case Result (interp-env e1 env sto)
                    [v*s*e (v1 s1 new-env) (interp-env e2 new-env s1)]
                    [Return (v1 s1 new-env) (Return v1 s1 new-env)]
                    [Break (s1 new-env) (Break s1 new-env)]
                    [Exception (v1 s1 new-env) (Exception v1 s1 new-env)])]
    
    ;; note that for now we're assuming that dict keys and values aren't going
    ;; to mess with the environment and store, but this might be wrong
    [CDict (contents)
           (letrec ([interped-hash (make-hash empty)]
                    [interp-pairs (lambda (lst)
                                  (map (lambda (pair)
                                       (hash-set! interped-hash
                                                   (v*s*e-v (interp-env (car pair) env sto))
                                                   (v*s*e-v (interp-env (cdr pair) env sto))))
                                      lst))])
             (begin
               (interp-pairs (hash->list contents))
               (v*s*e (VObject '$dict
                                (some (MetaDict interped-hash))
                                (make-hash empty))
                      sto env)))]

    [CSet (elts)
          (local [(define-values (result-list new-s new-e)
                                     (interp-cascade elts sto env))]
              (let ([exn? (filter Exception? result-list)])
                  (if (< 0 (length exn?))
                      (first exn?) 
                      (let ([val-list (map v*s*e-v result-list)])
                           (v*s*e (VObject 'set
                                           (some (MetaSet (make-set val-list)))
                                           (make-hash empty))
                                  new-s new-e)))))]

    [CList (values)
           (local [(define-values (result-list new-s new-e)
                                      (interp-cascade values sto env))]
               (let ([exn? (filter Exception? result-list)])
                   (if (< 0 (length exn?))
                       (first exn?)
                       (let ([val-list (map v*s*e-v result-list)])
                         (v*s*e (VObject 'list
                                         (some (MetaList val-list))
                                         (make-hash empty))
                                new-s
                                new-e)))))]

    [CTuple (values)
           (local [(define-values (result-list new-s new-e)
                                      (interp-cascade values sto env))]
               (let ([exn? (filter Exception? result-list)])
                   (if (< 0 (length exn?))
                       (first exn?) 
                      (let ([val-list (map v*s*e-v result-list)])
                       (v*s*e (VObject 'tuple
                                       (some (MetaTuple val-list))
                                       (make-hash empty))
                              new-s
                              new-e)))))]

    ;; only for ids!
    [CAssign (t v) (type-case Result (interp-env v env sto)
                     [v*s*e (vv sv venv)
                            (begin 
                              ;(if (and (CId? t) (symbol=? (CId-x t) 'x))
                              ;    (begin
                                    ;(display "assign: ") (display t) (display " ")
                                    ;(display vv) (display "\n")
                                    ;(display env) (display "\n\n"))
                              ;    (display ""))
                            (type-case CExpr t
                              [CId (x type) (assign-to-id t vv venv sv)]
                              [CGetField (o a) (assign-to-field o a vv venv sv)]
                              [else (mk-exception 'SyntaxError
                                                  "can't assign to literals"
                                                  venv
                                                  sv)]))]
                     [Return (vv sv ev) (return-exception ev sv)]
                     [Break (sv ev) (break-exception ev sv)]
                     [Exception (vv sv ev) (Exception vv sv ev)])]
    
    [CError (e) (type-case Result (interp-env e env sto)
                  [v*s*e (ve se ee)
                         (raise-user-error (pretty ve))]
                  [Return (ve se ee) (return-exception ee se)]
                  [Break (se ee) (break-exception ee se)]
                  [Exception (ve se ee) (Exception ve se ee)])]

    [CIf (i t e) (type-case Result (interp-env i env sto)
                       [v*s*e (vi si envi) (if (truthy? vi)
                                             (interp-env t envi si)
                                             (interp-env e envi si))]
                       [Return (vi si envi) (return-exception envi si)]
                       [Break (si envi) (break-exception envi si)]
                       [Exception (vi si envi) (Exception vi si envi)])]

    [CId (x t)
         (let ([result 
         (type-case IdType t
           [LocalId () 
             (let ([local-w (lookup-local x env)])
               (if (some? local-w)
                 (type-case CVal (fetch (some-v local-w) sto)
                   [VUndefined () (mk-exception 'UnboundLocalError 
                                                (string-append (symbol->string x)
                                                               " is undefined in this scope")
                                                env sto)]
                   [else (v*s*e (fetch (some-v local-w) sto) sto env)])
                 (let ([full-w (lookup x env)]
                       [name-error-str (string-append "name '" 
                                                     (string-append (symbol->string x)
                                                                    "' is not defined"))])
                   (if (some? full-w)
                     (type-case CVal (fetch (some-v full-w) sto)
                       [VUndefined () (mk-exception 'NameError name-error-str env sto)]
                       [else (v*s*e (fetch (some-v full-w) sto) sto env)])
                     (mk-exception 'NameError name-error-str env sto)))))]
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
                     [VUndefined () (mk-exception 'NameError name-error-str env sto)]
                     [else (v*s*e nonlocal-val sto env)]))
                 (local [(define syntax-error-str
                           (string-append "no binding for nonlocal '"
                                          (string-append (symbol->string x)
                                                         "' found")))]
                   (mk-exception 'SyntaxError syntax-error-str env sto))))]
           [GlobalId ()
             (local [(define full-w (lookup x env))
                     (define name-error-str
                       (string-append "name '"
                                      (string-append (symbol->string x)
                                                     "' is not defined")))]
               (if (some? full-w)
                 (local [(define full-val (fetch (some-v full-w) sto))]
                   (type-case CVal full-val
                     [VUndefined () (mk-exception 'NameError name-error-str env sto)]
                     [else (v*s*e full-val sto env)]))
                 (mk-exception 'NameError name-error-str env sto)))])])
             (if (symbol=? x 'x)
                 (begin
                   ;(display "id: ") (display x)
                   ;(display ", val: ") (display (v*s*e-v result)) (display "\n")
                   result)
                 result))]

    [CObject (c mval) (v*s*e (VObject c mval (hash empty))
                             sto
                             env)]

    [CLet (x bind body)
          (let ([w (new-loc)]
                [result (interp-env bind env sto)])
            (interp-let x result body))]

    [CApp (fun arges sarg) (local [(define result
          (interp-capp fun
                       arges
                       (if (none? sarg)
                         (some (CTuple empty))
                         sarg)
                       env
                       sto))]
                                  (begin ;(display "result: ") 
                                         ;(display (v*s*e-v result)) (display "\n\n")
                                         result))]

    [CFunc (args sargs body method?) (begin ;(display "func ") (display env) (display "\n\n")
           (v*s*e (VClosure (cons (hash empty) (if method?
                                                   (rest env)
                                                   env))
                            args sargs body)
                  sto env))]

    [CReturn (value) (type-case Result (interp-env value env sto)
                       [v*s*e (vv sv ev) (Return vv sv ev)]
                       [Return (vv sv ev) (return-exception ev sv)]
                       [Break (sv ev) (break-exception ev sv)]
                       [Exception (vv sv ev) (Exception vv sv ev)])]

    [CPrim1 (prim arg)
            (type-case Result (interp-env arg env sto)
              [v*s*e (varg sarg envarg) 
                   (case prim
                     ['Not (if (truthy? varg)
                             (v*s*e false-val sarg envarg)
                             (v*s*e true-val sarg envarg))]
                     [else (v*s*e (python-prim1 prim varg) sarg envarg)])]
              [Return (varg sarg earg) (return-exception earg sarg)]
              [Break (sarg earg) (break-exception earg sarg)]
              [Exception (varg sarg earg) (Exception varg sarg earg)])]

    [CWhile (body test orelse) (interp-while body test env sto)]
    
    ;; implement this
    [CPrim2 (prim arg1 arg2) (interp-cprim2 prim arg1 arg2 sto env)]
    
    [CBuiltinPrim (op args) (local [(define-values (result-list new-s new-e)
                                      (interp-cascade args sto env))]
                               (let ([exn? (filter Exception? result-list)])
                                 (if (< 0 (length exn?))
                                     (first exn?)
                                     (let ([val-list (map v*s*e-v result-list)])
                                       (local [(define mayb-val 
                                               (builtin-prim op val-list new-e new-s))] 
                                              (if (some? mayb-val)
                                       (v*s*e (some-v mayb-val)
                                              new-s
                                              new-e)
                                       ;; todo: more useful errors
                                       (mk-exception 'TypeError "Bad types in builtin call" env
                                                     sto)))))))]
    [CRaise (expr) 
            (if (some? expr)
                (type-case Result (interp-env (some-v expr) env sto)
                  [v*s*e (vexpr sexpr eexpr)
                         (cond
                           [(and (VObject? vexpr) (object-is? vexpr 'Exception env sto))
                            (Exception vexpr sexpr eexpr)]
                           [else (mk-exception 'TypeError
                                               "exceptions must derive from BaseException"
                                               eexpr
                                               sexpr)])]
                  [Return (vexpr sexpr eexpr) (return-exception eexpr sexpr)]
                  [Break (sexpr eexpr) (break-exception eexpr sexpr)]
                  [Exception (vexpr sexpr eexpr) (Exception vexpr sexpr eexpr)])
                (mk-exception 'RuntimeError
                              "No active exception to reraise"
                              env sto))]
    
    [CTryExceptElseFinally (try excepts orelse finally)
         (type-case Result (interp-env try env sto)
            [v*s*e (vtry stry etry)
                   (type-case Result (interp-env orelse etry stry)
                      [v*s*e (velse selse eelse)
                             (type-case Result (interp-env finally eelse selse)
                                [v*s*e (vfin sfin efin)
                                       (v*s*e vnone sfin efin)]
                                [Return (vfin sfin efin) (return-exception efin sfin)]
                                [Break (sfin efin) (Break sfin efin)]
                                [Exception (vfin sfin efin)
                                           (Exception vfin sfin efin)])]
                      [Return (velse selse eelse) (return-exception eelse selse)]
                      [Break (selse eelse) (Break selse eelse)]
                      [Exception (velse selse eelse)
                                 (Exception velse selse eelse)])]
            [Return (vtry stry etry) (Return vtry stry etry)]
            [Break (stry etry) (Break stry etry)]
            ;; handle excepts here
            [Exception (vtry stry etry)
               (local [(define result 
                         (if (empty? excepts)
                             (Exception vtry stry etry)
                             (interp-excepts excepts stry etry
                                             (Exception vtry stry etry))))]
                 (if (Exception? result)
                     (begin
                       (interp-env finally (Exception-e result) (Exception-s result))
                       result)
                     (if (Break? result)
                        result
                         (interp-env finally (v*s*e-e result) (v*s*e-s
                                                                result)))))])]

    [CExcept (types name body) (interp-env body env sto)]
    
    [CBreak () (Break sto env)]))


    ;[else (error 'interp "haven't implemented a case yet")]))

(define (assign-to-id id v e s)
  (local [(define-values (before scope after error)
            (type-case IdType (CId-type id)
              [GlobalId () (values (drop-right e 1) (last e) empty (none))]
              [NonlocalId ()
                (local [(define (find-scope-level [x : symbol] [env : Env] [idx : number])
                          (cond
                            [(empty? (rest env)) (none)]
                            [else (if (some? (hash-ref (first env) x))
                                      (some idx)
                                      (find-scope-level x (rest env) (add1 idx)))]))
                        (define level-idx (find-scope-level (CId-x id) (rest e) 1))]
                  (if (none? level-idx)
                      (values empty (hash empty) empty
                              (some
                                (mk-exception 'SyntaxError
                                   (string-append "no binding for nonlocal '"
                                                  (string-append (symbol->string (CId-x id))
                                                                 "' found"))
                                   e s)))
                       (local [(define-values (left right)
                                 (values (take e (some-v level-idx))
                                         (drop e (some-v level-idx))))]
                        (begin ;(display "left: ") (display left) (display "\n")
                               ;(display "right: ") (display right) (display "\n")
                              (values left (first right) (rest right) (none))))))]
              [LocalId () (values empty (first e) (rest e) (none))]))]
    (if (some? error)
        (some-v error)
        (local [(define mayb-w (hash-ref scope (CId-x id)))
                (define w (if (some? mayb-w) (some-v mayb-w) (new-loc)))]
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
                   (list (hash-set scope 
                             (CId-x id) w))
                   after)))))))

;; handles lookup chain for function calls on objects
;; looks in object dict, then class dict, then base class dicts, then default class
;; order in which base class dicts are traversed depends on truth value of super
;; depth-first, left-to-right if super = #f
;; left-to-right, depth-second if super = #t
(define (get-field [n : symbol] [c : CVal] [e : Env] [s : Store]) : Result
  (begin ;(display "GET: ") (display n) (display " ") (display c) (display "\n")
         ;(display e) (display "\n\n")
  (type-case CVal c
    [VObject (antecedent mval d) 
                    (let ([w (hash-ref (VObject-dict c) n)])
              (begin ;(display "loc: ") (display w) (display "\n\n")
                (type-case (optionof Address) w
                [some (w) (v*s*e (fetch w s) s e)]
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
                                                        e s)))]))])))]
    [else (error 'interp "Not an object with functions.")])))


(define (assign-to-field o f v e s)
  (type-case Result (interp-env o e s)
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
    [Return (vo so eo) (return-exception eo so)]
    [Break (so eo) (break-exception eo so)]
    [Exception (vo so eo) (Exception vo so eo)]))

(define (new-object [c-name : symbol] [e : Env] [s : Store])
  (VObject c-name (none) (hash empty)))

(define (bind-args [args : (listof symbol)] 
                   [sarg : (optionof symbol)]
                   [vals : (listof CVal)] 
                   [arges : (listof CExpr)] 
                   [env : Env] [ext : Env]
                   [sto : Store]) : (Env * Store * (optionof Result))
  (cond [(and (empty? args) (empty? vals)) 
              (if (some? sarg)
                  (bind-args (list (some-v sarg))
                             (none)
                             (list (make-builtin-tuple empty))
                             (list (make-builtin-num 0))
                             env 
                             ext
                             sto)
                  (values ext sto (none)))]
        ;need to bind star args!
        [(and (empty? args) (some? sarg)) 
         (let ([star-tuple (make-builtin-tuple vals)])
           (bind-args (list (some-v sarg))
                      (none) 
                      (list star-tuple)
                      arges env ext sto))]


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
                                             sto)))]
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
                             ;;[MetaDict (d) (;; get loc of val in store)]
                             ;; immutable types should get a new store location
                             [else (set! where (new-loc))]))
                         (set! where (mutability-check)))]
              [else (set! where (new-loc))])
            (let ([e (cons (hash-set (first ext) (first args) where) (rest ext))]
                  [s (hash-set sto where (first vals))])
                 (bind-args (rest args) sarg (rest vals) (rest arges) env e s))))]))


(define (mk-exception [type : symbol] [arg : string]
                      [env : Env] [sto : Store]) : Result
  (let ([exception 
          (interp-env (make-exception
                        type 
                        arg)
                      env sto)])
    (if (or (Exception? exception) (Break? exception))
               exception
               (Exception (v*s*e-v exception)
                          (v*s*e-s exception)
                          (v*s*e-e exception)))))

(define (return-exception [env : Env] [sto : Store]) : Result
  (mk-exception 'SyntaxError "'return' outside function" env sto))

(define (break-exception [env : Env] [sto : Store]) : Result
  (mk-exception 'SyntaxError "'break' outside loop" env sto))


(define (interp expr)
  (type-case Result (interp-env expr (list (hash (list))) (hash (list)))
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
    [VClosure (e a s b) true]
    [VObject (a mval d) (truthy-object? (VObject a mval d))]
    [VUndefined () false]))

(define (interp-cprim2 [prim : symbol] 
                       [arg1 : CExpr]
                       [arg2 : CExpr]
                       [sto : Store]
                       [env : Env]) : Result

    (type-case Result (interp-env arg1 env sto)
      [v*s*e (varg1 sarg1 envarg1)
           (type-case Result (interp-env arg2 envarg1 sarg1)
             [v*s*e (varg2 sarg2 envarg2) 
                  (case prim
                    ;; Handle Is, IsNot, In, NotIn
                    ['Is (if (is? varg1 varg2)
                           (v*s*e true-val sarg2 envarg2)
                           (v*s*e false-val sarg2 envarg2))]
                    ['IsNot (if (not (is? varg1 varg2))
                           (v*s*e true-val sarg2 envarg2)
                           (v*s*e false-val sarg2 envarg2))]
                    [else (error 'interp (string-append "Haven't implemented a case yet: "
                                                        (symbol->string
                                                          prim)))])]
             [Return (varg2 sarg2 envarg2) (return-exception envarg2 sarg2)]
             [Break (sarg2 envarg2) (break-exception envarg2 sarg2)]
             [Exception (varg2 sarg2 envarg2) (Exception varg2 sarg2 envarg2)])]
      [Return (varg1 sarg1 envarg1) (return-exception envarg1 sarg1)]
      [Break (sarg1 envarg1) (break-exception envarg1 sarg1)]
      [Exception (varg1 sarg1 envarg1) (Exception varg1 sarg1 envarg1)]))
