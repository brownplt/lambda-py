#lang plai-typed/untyped

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
         )

(define (handle-result result fun)
   (type-case Result result
     [v*s (v s a) (fun v s a)]
     [Return (v s a) (return-exception s)]
     [Break (s) (break-exception s)]
     [Continue (s) (continue-exception s)] 
     [Exception (v s) (Exception v s)]))

(define (append3 a b c)
  (append a (append b c)))

;; interp-cascade, interprets a list of expressions with an initial store,
;; environment and produces the list of results and the final store
(define (interp-cascade [exprs : (listof CExpr)] 
                        [init-s : Store]
                        [env : Env]
                        [stk : Stack]) : ((listof Result) * Store)
  (local [(define (rec-cascade exprs e s)
            (cond [(empty? exprs) empty]
                  [(cons? exprs)
                     (let ([first-r (interp-env (first exprs) e s stk)])
                       (type-case Result first-r
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

;; common code to interpret function and method application, first argument must be a VClosure.
(define (interp-vclosure [vfun : CVal] [arges : (listof CExpr)] 
                         [stararg : (optionof CExpr)]
                         [env : Env] [sfun : Store] [stk : Stack]) : Result
  (type-case CVal vfun
    [VClosure (cenv argxs vararg body opt-class)
              (local [(define-values (argvs-r sc) (interp-cascade arges sfun env stk))
                      (define exn? (filter Exception? argvs-r))]
                  (if (< 0 (length exn?))
                      (first exn?)
                      (local [(define argvs argvs-r)
                              (define result
                                (if (some? stararg)
                                    (local [(define sarg-r
                                              (interp-env (some-v stararg) env sc stk))
                                            ;; todo: support other types
                                            ;; for star args
                                            (define l (MetaTuple-v 
                                                        (some-v 
                                                          (VObjectClass-mval 
                                                            (v*s-v sarg-r)))))]
                                      (bind-and-execute 
                                        body opt-class argxs vararg 
                                        (append argvs (map (lambda (v)
                                                             (v*s v (v*s-s sarg-r) (none)))
                                                           l))
                                        (append arges (map (lambda(x)
                                                             (make-builtin-num 0))
                                                           l))
                                        env cenv (v*s-s sarg-r) stk)) 
                                    (bind-and-execute
                                      body opt-class argxs vararg
                                      argvs arges env
                                      cenv sc stk)))]
                        (type-case Result result
                          [v*s (vb sb ab) (v*s vnone sb (none))]
                          [Return (vb sb ab) (v*s vb sb ab)]
                          [Break (sb) (break-exception sb)]
                          [Continue (sb) (continue-exception sb)]
                          [Exception (vb sb) (Exception vb sb)]))))]
    [else (error 'interp (string-append "Not a closure: " (to-string vfun)))]))

(define (interp-capp [fun : CExpr] [arges : (listof CExpr)] 
                     [stararg : (optionof CExpr)]
                     [env : Env] [sto : Store] [stk : Stack]) : Result
  (begin ;(display "APP: ") (display fun) (display "\n") (display arges) (display "\n\n\n")
         ;(display env) (display "\n\n")
 (handle-result (interp-env fun env sto stk)
  (lambda (vfun sfun afun)
    (type-case CVal vfun
      [VClosure (cenv argxs vararg body opt-class)
                (interp-vclosure vfun arges stararg env sfun stk)]

      [VObjectClass (b mval d class)
               (if (and (some? mval) (MetaClass? (some-v mval)))
                  ; We're calling a class.
                  ; Get its constructor
                  (local [(define f (v*s-v (get-field '__init__ vfun (none) env sfun)))
                          ; Create an empty object. This will be the instance of that class.
                          (define loc (new-loc))
                          (define nid (new-id))
                          (define o (new-object (MetaClass-c (some-v mval)) afun))]
                    (type-case CVal f
                      [VClosure (cenv argxs vararg body opt-class)
                                ; interpret the arguments to the constructor
                         (local [(define-values (argvs-r sc) (interp-cascade arges sfun env stk))
                                 (define exn? (filter Exception? argvs-r))]
                           (if (< 0 (length exn?))
                               (first exn?)
                               (local [(define argvs argvs-r)
                                       (define sto-with-obj (hash-set sc loc o))
                                       (define obj (v*s o sto-with-obj (some loc)))
                                       ; bind the interpreted arguments to the constructor
                                       (define result (bind-and-execute
                                                        body opt-class argxs vararg
                                                        (cons obj argvs)
                                                        (cons (CId (new-id) (LocalId)) arges)
                                                        (cons (hash-set (first env) nid loc)
                                                              (rest env))
                                                        cenv
                                                        sto-with-obj 
                                                        stk))]
                                 (type-case Result result
                                   [v*s (vb sb ab) (v*s (fetch loc sb) sb (none))]
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
                      [else (error 'interp "__init__ not found. THIS SHOULDN'T HAPPEN.")]))
                   
                  ;; This means we're calling an object,
                  ;; so apply its __call__ method.
                  (local [(define __call__ (get-field '__call__ vfun (none) env sfun))]
                    (type-case Result __call__
                      [v*s (vc sc ac)
                           (cond
                             ;; for bound methods use __func__ attribute and __self__
                             [(and (VObjectClass? vc) (equal? (VObjectClass-antecedent vc) 'method))
                              (local 
                                [(define func
                                   (fetch (some-v (hash-ref (VObjectClass-dict vc) '__func__)) sc))
                                 (define w_self (hash-ref (VObjectClass-dict vc) '__self__))
                                 (define id_self (new-id))
                                 (define m_arges (cons (CId id_self (LocalId)) arges))
                                 ;; extend the environment with self to support self aliasing
                                 (define m_env
                                   (cons (hash-set (first env) id_self (some-v w_self)) 
                                         (rest env)))]
                                (begin
                                  ;(display (format "Method is: ~a\n" vc))
                                  ;(display (format "Func is: ~a\n" func))
                                  (interp-vclosure func m_arges stararg m_env sc stk)))]
                             [else
                               ;; for unbound methods, use function application
                               (interp-vclosure vc arges stararg env sfun stk)])]
                      [Return (vfun sfun afun) (return-exception sfun)]
                      [Break (sfun) (break-exception sfun)]
                      [Continue (sfun) (continue-exception sfun)]
                      [Exception (vfun sfun) (mk-exception 'TypeError
                                                           (string-append 
                                                             (symbol->string b)
                                                             " object is not callable")
                                                           sto)])))]
      [else (error 'interp "Not a closure or constructor.")])))))

(define (interp-while [test : CExpr] [body : CExpr] [orelse : CExpr]
                      [env : Env] [sto : Store] [stk : Stack]) : Result
  (local [(define test-r (interp-env test env sto stk))]
    ;; if test results in an exception, pass it along
    (if (Exception? test-r)
        test-r
        (if (truthy? (v*s-v test-r))
            (local [(define body-r (interp-env body env (v*s-s test-r) stk))]
              (cond
                ;; if the body results in an exception of return, pass it along
                [(or (Exception? body-r) (Return? body-r)) body-r]
                ;; if it results in a break, return None
                [(Break? body-r) (v*s vnone (Break-s body-r) (none))]
                ;; if it resulted in a value or continue, attempt to run the loop again
                [else (interp-while test body orelse env (v*s-s body-r) stk)]))
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
                      (some (fetch (some-v (lookup (some-v opt-class) ext)) sto))
                      (none)))
                (define self 
                  (if (and (some? opt-class) (> (length argvs) 0))
                      ;; self is the first argument, if any, for methods
                      (some (v*s-v (first argvs)))
                      (none)))]
        (interp-env body env_new sto_new 
                    ;; push new activation record on the stack
                    ;; used the dynamic environment for compatibility with base code.
                    ;; TODO(joe): env_new vs env in the Frame?  We lost dyn in
                    ;; the update for scope
                    ;;
                    ;; As far as I could tell, dyn was a copy of env, so I got rid of it.
                    ;; I think env is right here.
                    ;; - Sumner
                    (cons (Frame env class self) stk))))))

(define (interp-excepts [excepts : (listof CExpr)]
                        [sto : Store]
                        [env : Env]
                        [exn : Result]
                        [stk : Stack]) : Result
  (local [(define exn-type (VObjectClass-antecedent (Exception-v exn)))
          (define (find-match type exps fsto)
            (cond
              [(empty? exps) (values (none) fsto (none))]
              [(cons? exps)
               ;; need to interp exprs and then check
               (local [(define-values (except-types-results tsto)
                         (interp-cascade (CExcept-types (first exps)) fsto env stk))
                       (define exn? (filter Exception? except-types-results))]
                 (if (< 0 (length exn?))
                     (values (none) tsto (some (first exn?)))
                     (local [(define actual-except-types
                               (if (and (> (length except-types-results) 0)
                                        (VObjectClass? (v*s-v (first except-types-results)))
                                        (MetaTuple?
                                          (some-v (VObjectClass-mval
                                                    (v*s-v (first except-types-results))))))
                                   (map (lambda (v) (v*s v
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
            (local [(define as-name (CExcept-name (some-v match?)))
                    (define result
                      (if (some? as-name)
                          (interp-let (some-v as-name)
                                ;; This could potentially be in global scope. Does it matter?
                                (LocalId)
                                (Exception (Exception-v exn) hsto)
                                (CExcept-body (some-v match?))
                                env stk)
                          (interp-env (some-v match?) env hsto stk)))]
              (type-case Result result
                [v*s (vbody sbody abody) (v*s vnone sbody (none))]
                [Return (vbody sbody abody) (Return vbody sbody abody)]
                [Break (sbody) (Break sbody)]
                [Continue (sbody) (Continue sbody)] 
                [Exception (vbody sbody)
                           (local [(define exn-args (hash-ref (VObjectClass-dict vbody) 'args))
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
                                 result))]))
          (Exception (Exception-v exn) hsto)))))

(define (interp-let [name : symbol] [type : IdType] [value : Result]
                    [body : CExpr] [env : Env] [stk : Stack]) : Result
  (local [(define-values (val sto)
            (type-case Result value
              [v*s (v s a) (values v s)]
              [Return (v s a) (values v s)]
              [Break (s) (values vnone s)]
              [Continue (s) (values vnone s)] 
              [Exception (v s) (values v s)]))
          (define loc (new-loc))
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
                (hash-set sto loc val)
                stk)))

;; interp-id will first lookup id in env, then fetch the value of the id in the sto.
;; At the same time, interp-id will return the address of the id for aliasing.
;; NOTE: for aliasing id, which has the value of (VPointer original-addr), the
;; returned address will be the original one, not the address of the VPointer. The
;; deep-lookup-* will obtain the original address of an identifier.
;; --Junsong
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
               (local [(define local-w (deep-lookup-local id env sto))]
                 (if (some? local-w)
                     (type-case CVal (fetch (some-v local-w) sto)
                       [VUndefined () (mk-exception 'UnboundLocalError
                                                    unboundlocal-error-str
                                                    sto)]
                       [else (v*s (fetch (some-v local-w) sto) sto local-w)])
                     (local [(define full-w (deep-lookup id env sto))]
                       (if (some? full-w)
                           (local [(define full-val (fetch (some-v full-w) sto))]
                             (type-case CVal full-val
                               [VUndefined () (mk-exception 'NameError freevar-error-str sto)]
                               [else
                                 (if (immutable-type? full-val)
                                     ;; if immutable don't return store location
                                     (v*s full-val sto (none))
                                     (v*s full-val sto full-w))]))
                           (mk-exception 'NameError
                                         (string-append "global " name-error-str)
                                         sto)))))]
      [GlobalId ()
                (local [(define full-w (deep-lookup-global id env sto))]
                  (if (some? full-w)
                      (local [(define full-val (fetch (some-v full-w) sto))]
                        (type-case CVal full-val
                          [VUndefined () (mk-exception 'NameError name-error-str sto)]
                          [else
                            (if (immutable-type? full-val)
                                ;; if immutable don't return store location
                                (v*s full-val sto (none))
                                (v*s full-val sto full-w))]))
                      (mk-exception 'NameError name-error-str sto)))])))

;; interp-env : CExpr * Env * Store * Stack -> Result
(define (interp-env [expr : CExpr] [env : Env] [sto : Store] [stk : Stack]) : Result
  (begin ;(display expr) (display "\n")
         ;(display env) (display "\n\n")
  (type-case CExpr expr
    [CModule (prelude body)
             (local [(define prelude-r (interp-env prelude env sto stk))]
                (handle-result prelude-r
                  (lambda (v s a) (interp-env body env s stk))))]
    
    ;; TODO(joe): first non in VOBjectClass below?
    ;;
    ;; The (none) is fine here because we aren't placing the object in the store yet.
    ;; - Sumner
    [CStr (s) (v*s (VObjectClass 'str (some (MetaStr s)) (hash empty) (none)) sto (none))]
    [CTrue () (v*s true-val sto (none))]
    [CFalse () (v*s false-val sto (none))]
    [CNone () (v*s vnone sto (none))]
    [CUndefined () (v*s (VUndefined) sto (none))]

    [CClass (name bases body)
            ;; the tuple of bases is evaluated assuming global scope for class names,
            ;; should be changed for a more general lookup with the new scope implementation
            (begin ;(display "BEGIN CLASS\n") (display bases)
            ;; NOTE(joe): weird CTuple of none, probably should be constructed
            ;; in desugaring.
            (handle-result (interp-env (CTuple (CNone)
                                               (map (lambda (id) (CId id (GlobalId)))
                                                       bases))
                                          env sto stk)
              (lambda (vbases sbases abases)
                   (handle-result (interp-env body (cons (hash empty) env) sto stk)
                     (lambda (vbody sbody abody)
                          (begin ;(display name) (display "\n")
                                 ;(display env) (display "\n")
                                 ;(display sbody) (display "\n")
                                 (let ([res (mk-type name vbases (hash empty) sbody env)])
                                   res)))))))]
   
    [CGetField (value attr)
               (handle-result (interp-env value env sto stk)
                          (lambda (vval sval aval) (get-field attr vval aval env sval)))]
			
    [CSeq (e1 e2) (type-case Result (interp-env e1 env sto stk)
                    [v*s (v1 s1 a1) (interp-env e2 env s1 stk)]
                    [Return (v1 s1 a1) (Return v1 s1 a1)]
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
                                                   (v*s-v (interp-env (car pair) env sto stk))
                                                   (v*s-v (interp-env (cdr pair) env sto stk))))
                                      lst))])
             (begin
               (interp-pairs (hash->list contents))
               ;; TODO(joe): none in VObjectClass again
               ;;
               ;; Fine again here; not placing it in the store;
               ;; - Sumner
               (v*s (VObjectClass '$dict
                              (some (MetaDict interped-hash))
                              (hash empty)
                              (none))
                    sto
                    (none))))]

    [CSet (elts)
          (local [(define-values (result-list new-s) (interp-cascade elts sto env stk))]
              (let ([exn? (filter Exception? result-list)])
                  (if (< 0 (length exn?))
                      (first exn?) 
                      (let ([val-list (map v*s-v result-list)])
                           ;; TODO(joe): none in VObjectClass again
                           ;; 
                           ;; and again.
                           ;; - Sumner
                           (v*s (VObjectClass 'set
                                         (some (MetaSet (make-set val-list)))
                                         (hash empty)
                                         (none))
                                new-s
                                (none))))))]
    
    [CList (class values)
     (local [(define-values (result-list new-s) (interp-cascade values sto env stk))]
         (let ([exn? (filter Exception? result-list)])
             (if (< 0 (length exn?))
                 (first exn?) 
                 (handle-result (interp-env class env new-s stk)
                  (lambda (cval csto cloc)
                (let ([val-list (map v*s-v result-list)])
                 (v*s (VObjectClass 'list
                               (some (MetaList val-list))
                               (hash empty)
                               (some cval))
                      csto
                      (none))))))))]

    [CTuple (class values)
     (local [(define-values (result-list new-s) (interp-cascade values sto env stk))]
         (let ([exn? (filter Exception? result-list)])
             (if (< 0 (length exn?))
                 (first exn?) 
                 (handle-result (interp-env class env new-s stk)
                  (lambda (cval csto cloc)
                (let ([val-list (map v*s-v result-list)])
                 (v*s (VObjectClass 'tuple
                               (some (MetaTuple val-list))
                               (hash empty)
                               (some cval))
                      csto
                      (none))))))))]

    [CAssign (t v) 
             (begin ;(display "\nASSIGN: ") (display t) (display " | ") (display v) (display "\n")
             (local [(define val (interp-env v env sto stk))]
               (handle-result val
                 (lambda (vv sv av)
                      (type-case CExpr t
                        [CId (x type) (assign-to-id t val env sv)]
                        [CGetField (o a) (assign-to-field o a val env sv stk)]
                        [else (mk-exception 'SyntaxError
                                            "can't assign to literals"
                                            sv)])))))]
    
    [CIf (i t e) (handle-result (interp-env i env sto stk)
                   (lambda (vi si ai) (if (truthy? vi)
                                       (interp-env t env si stk)
                                       (interp-env e env si stk))))]
    
    [CId (x t) (interp-id x t env sto)]

    [CObject (c mval)
     (v*s (VObjectClass c mval (hash empty) (none)) sto (none))]

    [CLet (x type bind body)
          (begin ;(display "LET: ") (display x) (display " ")
                 ;(display type) (display bind) (display "\n")
          (interp-let x type (interp-env bind env sto stk) body env stk))]

    [CApp (fun arges sarg)
          (begin ;(display "CApp") (display fun) (display arges) (display "\n")
          (interp-capp fun arges
                       (if (none? sarg)
                           (some (CTuple (CNone) empty))
                           sarg)
                       env sto stk))]

    [CFunc (args sargs body opt-class) 
           (begin ;(display "func ") (display env) (display "\n\n")
           (v*s (VClosure
                  (cons (hash empty) env)
                  ;(if (some? opt-class) (rest env) env)
                  args sargs body opt-class)
                sto
                (none)))]

    [CReturn (value)
             (handle-result (interp-env value env sto stk)
               (lambda (vv sv av) (Return vv sv av)))]

    [CPrim1 (prim arg)
            (handle-result (interp-env arg env sto stk)
              (lambda (varg sarg aarg) 
                   (case prim
                     ['Not (if (truthy? varg)
                             (v*s false-val sarg (none))
                             (v*s true-val sarg (none)))]
                     [else (v*s (python-prim1 prim varg) sarg (none))])))]

    [CWhile (body test orelse) (interp-while body test orelse env sto stk)]

    [CPrim2 (prim arg1 arg2) (interp-cprim2 prim arg1 arg2 sto env stk)]
    
    [CBuiltinPrim (op args)
                  (local [(define-values (result-list new-s) (interp-cascade args sto env stk))
                          (define exn? (filter Exception? result-list))]
                    (if (< 0 (length exn?))
                        (first exn?)
                        (local [(define val-list (map v*s-v result-list))
                                (define mayb-val (builtin-prim op val-list env new-s stk))] 
                          (if (some? mayb-val)
                              ;; BuiltinPrims should return Results, not (optionof CVal)
                              (v*s (some-v mayb-val) new-s (none))
                              ;; todo: more useful errors
                              (mk-exception 'TypeError "Bad types in builtin call" 
                                            sto)))))]
    [CRaise (expr) 
            (if (some? expr)
                (handle-result (interp-env (some-v expr) env sto stk)
                  (lambda (vexpr sexpr aexpr)
                       (cond
                         [(and (VObjectClass? vexpr) (object-is? vexpr 'Exception env sto))
                          (Exception vexpr sexpr)]
                         [else (mk-exception 'TypeError
                                             "exceptions must derive from BaseException"
                                             sexpr)])))
                (mk-exception 'RuntimeError
                              "No active exception to reraise"
                              sto))]
    
    ;; revisit this, I think there are some errors with finally (Sumner)
    ;; TODO(joe): Who wrote the above?  Please sign your comments.  What
    ;; finally tests do you fear failures on?
    [CTryExceptElseFinally (try excepts orelse finally)
         (type-case Result (interp-env try env sto stk)
            [v*s (vtry stry atry)
                   (type-case Result (interp-env orelse env stry stk)
                      [v*s (velse selse aelse)
                             (type-case Result (interp-env finally env selse stk)
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
            ;; handle excepts here
            [Exception (vtry stry)
               (local [(define result 
                         (if (empty? excepts)
                             (Exception vtry stry)
                             (interp-excepts excepts stry env (Exception vtry stry) stk)))]
                 ;; TODO(Sumner): make sure this is still right!
                 ;; TODO(Sumner): It's not. Fix it.
                 ;; TODO(joe): Please sign your comments so we know who to
                 ;; contact with questions.  It's fine that it doesn't work,
                 ;; but everyone should know who to ask for how to get started
                 ;; fixing it!
                 (type-case Result result
                   [v*s (vexc sexc aexc) (interp-env finally env sexc stk)]
                   ;; finally block is excecuted, but side effects are ignored...
                   [Return (vexc sexc aexc) (begin (interp-env finally env sexc stk) result)]
                   [Break (sexc) (begin (interp-env finally env sexc stk) result)]
                   [Continue (sexc) (begin (interp-env finally env sexc stk) result)]
                   [Exception (vexc sexc) (begin (interp-env finally env sexc stk) result)]))])]

    [CExcept (types name body) (interp-env body env sto stk)]
    
    [CBreak () (Break sto)]
    [CContinue () (Continue sto)])))

(define (assign-to-id [id : CExpr] [val : Result] [env : Env]
                      [sto : Store]) : Result
  (local [(define mayb-loc 
            (type-case IdType (CId-type id)
              [LocalId () (deep-lookup (CId-x id) env sto)]
              [GlobalId () (deep-lookup-global (CId-x id) env sto)]))
          (define value (if (v*s? val)
                            (if (some? (v*s-a val))
                                (VPointer (some-v (v*s-a val)))
                                (v*s-v val))
                            (if (some? (Return-a val))
                                (VPointer (some-v (Return-a val)))
                                (Return-v val))))]
(begin ;(display "mayb-loc:") (display  mayb-loc) (display "\n")
       ;(display "before assign, the store:")
       ;(if (some? mayb-loc) (pprint (fetch-once (some-v mayb-loc) sto)) (pprint "OH NO"))
       ;(display "after assign, the store:")
       ;(if (some? mayb-loc) (pprint value) (pprint "OLD STO"))
       ;(display "\n")
  (if (some? mayb-loc)
      (v*s vnone (hash-set sto (some-v mayb-loc) value) (none))        
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
                                            sto)])))))

(define (global-scope? [env : Env]) : boolean
  (= (length env) 1))

;; handles lookup chain for function calls on objects
;; multiple inheritance modification : for class lookup call get-field-from-class
;; optional address field added to support self aliasing in bound methods calls.
(define (get-field [n : symbol] [c : CVal] [w_c : (optionof Address)] 
                   [e : Env] [s : Store]) : Result
  (begin ;(display "GET: ") (display n) (display " ") (display c) 
         ;(display " ") (display w_c) (display "\n\n")
         ;(display e) (display "\n\n")
    (cond
      [(not (VObjectClass? c))
       (mk-exception 'AttributeError
                     (string-append 
                      (string-append (pretty c) " object has no attribute ")
                      (symbol->string n))
                     s)]
      [(is-special-method? n)
       ;; special methods are looked for in the class
       (get-field-from-obj n c w_c (none) e s)]
      [(and (some? (VObjectClass-mval c)) (MetaClass? (some-v (VObjectClass-mval c))))
       ;; class lookup
       (get-field-from-cls n c w_c (none) e s)]
      [else
        ;; instance lookup
        (type-case CVal c
          [VObjectClass (antecedent mval d class) 
                   (let ([w (hash-ref (VObjectClass-dict c) n)])
                     (begin ;(display "loc: ") (display w) (display "\n\n")
                       (type-case (optionof Address) w
                         [some (w) 
                               (v*s (fetch w s) s (some w))]
                         [none () (get-field-from-obj n c w_c (none) e s)])))]
          [else (error 'interp "Not an object with fiedls.")])])))

(define (assign-to-field o f v [env : Env] [sto : Store] [stk : Stack]) : Result
  (begin ;(display o) (display "---") (display f) (display "\n")
  (handle-result (interp-env o env sto stk)
    (lambda (vo so ao)
         (type-case CVal vo
           [VObjectClass (antecedent mval d class)
             (local [(define loc (hash-ref (VObjectClass-dict vo) f))
                     (define value (if (v*s? v)
                                       (if (some? (v*s-a v))
                                           (VPointer (some-v (v*s-a v)))
                                           (v*s-v v))
                                       (if (some? (Return-a v))
                                           (VPointer (some-v (Return-a v)))
                                           (Return-v v))))]
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
                                              (VObjectClass antecedent
                                                       mval
                                                       (hash-set (VObjectClass-dict vo) f w)
                                                       class))))] ;; NOTE(joe) ensuring same class as above
                              (v*s vnone
                                   (hash-set snew w value)
                                   (none)))]))]
           [else (error 'interp "Can't assign to nonobject.")])))))

(define (new-object [c-name : symbol] [c-loc : (optionof Address)]) : CVal
  (VObjectClass c-name (none) (hash empty) (some (VPointer (some-v c-loc)))))

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
         (if (some? sarg)
           (bind-args (list (some-v sarg))
                      (none)
                      (list (v*s (make-builtin-tuple empty) sto (none)))
                      (list (make-builtin-num 0))
                      env ext sto)
           (values ext sto (none)))]
        ;need to bind star args!
        [(and (empty? args) (some? sarg)) 
         ;; This means we have a stararg symbol and extra arguments, so we'll
         ;; place them in a tuple and bind it to the stararg symbol.
         (let ([star-tuple (make-builtin-tuple (map v*s-v vals))])
           (bind-args (list (some-v sarg))
                      (none) 
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
                                         (to-string (map v*s-v vals)))
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
                         ;; TODO(Sumner): better mutability check?
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
                 ; TODO(Sumner): why env and not ext here?
                 (define s (hash-set sto loc vv))]
                (bind-args (rest args) sarg (rest vals) (rest arges) env e s))]))

(define (mk-exception [type : symbol] [arg : string] [sto : Store]) : Result
  (local [(define loc (new-loc))
          (define args (list (VObjectClass 'str (some (MetaStr arg)) (hash empty) (none))))]
    (Exception
      (VObjectClass type (none) (hash-set (hash empty) 'args loc) (none))
      (hash-set sto loc (VObjectClass 'tuple (some (MetaTuple args)) (hash empty) (none))))))

(define (return-exception [sto : Store]) : Result
  (mk-exception 'SyntaxError "'return' outside function" sto))

(define (break-exception [sto : Store]) : Result
  (mk-exception 'SyntaxError "'break' outside loop" sto))

(define (continue-exception [sto : Store]) : Result
  (mk-exception 'SyntaxError "'continue' outside loop" sto))

(define (interp expr)
(begin
  (reset-loc)
  (type-case Result (interp-env expr (list (hash empty)) (hash empty) empty)
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
               (raise-user-error (string-append (pretty-exception vexpr sexpr) "\n"))])))

(define (truthy? [val : CVal]) : boolean
  (type-case CVal val
    [VClosure (e a s b o) true]
    [VObjectClass (a mval d class) (truthy-object? (VObjectClass a mval d class))]
    [VUndefined () false]
    [else (error 'truthy? "Shouldn't check truthiness of Pointer.")]))

(define (interp-cprim2 [prim : symbol] 
                       [arg1 : CExpr]
                       [arg2 : CExpr]
                       [sto : Store]
                       [env : Env]
                       [stk : Stack]) : Result
    (handle-result (interp-env arg1 env sto stk)
      (lambda (varg1 sarg1 aarg1)
           (handle-result (interp-env arg2 env sarg1 stk)
             (lambda (varg2 sarg2 aarg2) 
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
                                                          prim)))]))))))

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
    (handle-result (build-mro name bases-list env sto)
      (lambda (vmro smro amro) 
             (v*s (VObjectClass 'type
                           (some (MetaClass name)) 
                           (hash-set
                             (hash-set 
                               (hash-set 
                                 (hash-set h-dict '__dict__ w)
                                 '__bases__ bases_w)
                               '__mro__ mro_w)
                             '__class__ class_w)
                           (none))
                  (hash-set 
                    (hash-set 
                      (hash-set sto w (make-under-dict h-dict sto))
                      bases_w bases)
                    mro_w vmro)
                  (none))))))

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

         ;; TODO(joe): the (none) at the end of VObjectClass is questionable
         ;; here, as well, because I'm touching code I don't fully understand
         ;;
         ;; The last field in VObjectClass an optionof location of the class of
         ;; the object in the store. Here, it would be the store location of the
         ;; object representing the class 'tuple'. We decided to maintain an invariant
         ;; that all objects placed into the store would have this field filled in.
         ;; That means it's fine to have (none) here because we are only creating an
         ;; object; we're not placing it in the store right now. It will get filled in
         ;; elsewhere.
         ;; - Sumner
         (v*s (VObjectClass 'tuple (some (MetaTuple (some-v maybe-mro))) (hash empty) (none))
              sto
              (none)))])))
 
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
                            [obj : CVal]
                            [w_obj : (optionof Address)]
                            [thisclass : (optionof CVal)]
                            [env : Env] 
                            [sto : Store]) : Result
  (begin ;(display "GET-OBJ: ") (display fld) (display " ") (display obj) 
         ;(display " ") (display w_obj) (display "\n")
  (cond
    ;; for method objects, __call__ attribute is the object itself
    [(and (equal? (VObjectClass-antecedent obj) 'method) (equal? fld '__call__))
     (v*s obj sto w_obj)]
    ;; special lookup handling for initialized super object
    [(and (equal? (VObjectClass-antecedent obj) 'super)
          (some? (hash-ref (VObjectClass-dict obj) '__self__)))
     (local ([define w_self (hash-ref (VObjectClass-dict obj) '__self__)]
             [define self (fetch (some-v w_self) sto)]
             [define thisclass (fetch (some-v (hash-ref (VObjectClass-dict obj)
                                                        '__thisclass__))
                                      sto)])
       (cond
         [(and (VObjectClass? self) (equal? (VObjectClass-antecedent self) 'type))
          ;; obj.self is a class
          (get-field-from-cls fld self w_self (some thisclass) env sto)]
         [else
          ;; obj.self is an instance
          (get-field-from-obj fld self w_self (some thisclass) env sto)]))]
    ;; normal instance lookup
    [else
     (local ([define obj-cls (get-class obj env sto)])
       (type-case (optionof Address) (lookup-mro (get-mro obj-cls thisclass sto) fld)
         [some (w) (let ([value (fetch w sto)])
                     (cond
                       ;; For functions, create method object bound to the object itself
                       [(VClosure? value) 
                        (local [(define-values (meth sto-m) (mk-method w obj w_obj sto))]
                          (v*s meth sto-m (none)))]
                       ;; for classmethod objects create method object bound to the object's class
                       [(and (VObjectClass? value) 
                             (equal? (VObjectClass-antecedent value) 'classmethod))
                        (local [(define w_func 
                                  (some-v (hash-ref (VObjectClass-dict value) '__func__)))
                                (define-values (meth sto-m) 
                                  (mk-method w_func obj-cls (none) sto))]
                          (v*s meth sto-m (none)))]
                       ;; for staticmethod obj. return func attribute
                       [(and (VObjectClass? value) 
                             (equal? (VObjectClass-antecedent value) 'staticmethod))
                                  (get-field '__func__ value (none) env sto)]
                       ;; otherwise return the value of the attribute
                       [else 
                        (v*s value sto w)]))]
         [none () (mk-exception 'AttributeError
                                (string-append 
                                 (string-append "object " 
                                                (symbol->string (VObjectClass-antecedent obj)))
                                 (string-append " has no attribute "
                                                (symbol->string fld)))
                                sto)]))])))

;; get-field-from-cls: looks for a field of a class using class __mro__
;; skip up to thisclass in __mro__, if defined.
;; optional address field added to support self aliasing in bound methods calls.
(define (get-field-from-cls [fld : symbol] 
                            [cls : CVal]
                            [w_cls : (optionof Address)]
                            [thisclass : (optionof CVal)]
                            [env : Env] 
                            [sto : Store]) : Result
  (cond 
    [(equal? fld '__mro__) 
     ;; temporary hack to avoid self-reference in __mro__
     (v*s (VObjectClass 'tuple (some (MetaTuple (get-mro cls thisclass sto))) (hash empty) (none))
          sto
          (none))]
    [else
     (type-case (optionof Address) (lookup-mro (get-mro cls thisclass sto) fld)
       [some (w)
             (let ([value (fetch w sto)])
               (cond
                 ;; for classmethod obj. create method obj. bound to the class
                 [(and (VObjectClass? value) 
                       (equal? (VObjectClass-antecedent value) 'classmethod))
                  (local [(define w_func (some-v (hash-ref (VObjectClass-dict value) '__func__)))
                          (define-values (meth sto-m) (mk-method w_func cls w_cls sto))]
                    (v*s meth sto-m (none)))]
                 ;; for staticmethod obj. return func attribute
                 [(and (VObjectClass? value)
                       (equal? (VObjectClass-antecedent value) 'staticmethod))
                  (get-field '__func__ value (none) env sto)]
                 ;; otherwise return the value of the attribute
                 [else
                  (v*s value sto (some w))]))]
       [none () (mk-exception 'AttributeError
                              (string-append 
                               (string-append "class " 
                                              (symbol->string (VObjectClass-antecedent cls)))
                               (string-append " has no attribute "
                                              (symbol->string fld)))
                              sto)])]))

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

;; special methods
(define (is-special-method? [n : symbol])
  (member n (list '__eq__ '__cmp__ '__str__ '__getitem__ '__gt__ '__lt__ '__lte__ '__gte__)))
