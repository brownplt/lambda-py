#lang plai-typed

(require "python-core-syntax.rkt"
         "python-syntax.rkt"
         "python-primitives.rkt"
         "python-desugar.rkt")

(require (typed-in racket/base [string<? : [string string -> boolean]]))
(require (typed-in racket/base [string>? : [string string -> boolean]]))
(require (typed-in racket/base [string<=? : [string string -> boolean]]))
(require (typed-in racket/base [string>=? : [string string -> boolean]]))
(require (typed-in racket/base [string-length : [string -> number]]))
(require (typed-in racket/base [bitwise-not : [number -> number]]))
(require (typed-in racket/base [fixnum? : [number -> boolean]]))
(require (typed-in racket/base [flonum? : [number -> boolean]]))
;(require (typed-in racket/string [string-split : [string -> (listof string)]]))
(require (typed-in racket/base [string->list : [string -> (listof string)]]))
(require (typed-in racket/base [list->string : [(listof string) -> string]]))
(require (typed-in racket/base [list-tail : [(listof 'a) number -> (listof 'a)]]))
(require (typed-in racket/list [argmax : [('a -> number) (listof 'a) -> 'a]]))
(require (typed-in racket/list [drop-right : [(listof 'a) number -> (listof 'a)]]))
(require (typed-in racket/list [last : [(listof 'a) -> 'a]]))
(require (typed-in racket/base [substring : [string number number -> string]]))

;;Holds the exception to be reraised
(define exn-to-reraise
  (box (VUnbound)))

;;Returns a new memory address to be used
(define new-loc
  (let ([n (box 1)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

;;Returns a new uid to be used
(define new-uid
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

;;this is the global variable with the global environment
(define globalEnv
  (hash (list)))

;;this should be called only once, at the beggining of the interpretation, 
;;to copy the initial environment and create the global one
(define (createGlobalEnv [env : Env]) : Env
  (foldl (lambda (key newEnv) 
           (type-case (optionof SLTuple) (hash-ref env key)
             [none () (error 'createGlobalScope "Cannot find key inside hash with this key in hash-keys: something is very wrong")]
             [some (v) (local [(define-values (t l) v)]
                         (augmentEnv key (values (Global) l) newEnv))]))
         (hash (list))
         (hash-keys env)))

;;keepOldEnv is a helper function that will keep all of the non-global variables of the older scope,
;;remembering to change 'Local' variables into 'NonLocal' ones
(define (keepOldEnv [env : Env]) : Env
  (foldl (lambda (key newEnv) 
           (type-case (optionof SLTuple) (hash-ref env key)
             [none () (error 'keepOldEnv "Cannot find key inside hash with this key in hash-keys: something is very wrong")]
             [some (v) (local [(define-values (t l) v)]
                         (type-case ScopeType t
                         ;  [Instance () newEnv]
                           [Local () (augmentEnv key (values (NonLocal) l) newEnv)]
                           ;[NotReallyLocal () (augmentEnv key (values (NonLocal) l) newEnv)]
                           [Global () newEnv]
                           [NonLocal () (augmentEnv key (values (NonLocal) l) newEnv)]))]))
         (hash (list))
         (hash-keys env)))

(define (keepOldEnvClass [env : Env]) : Env
  (foldl (lambda (key newEnv) 
           (type-case (optionof SLTuple) (hash-ref env key)
             [none () (error 'keepOldEnvClass "Cannot find key inside hash with this key in hash-keys: something is very wrong")]
             [some (v) (local [(define-values (t l) v)]
                         (type-case ScopeType t
                          ; [Instance () (augmentEnv key (values (Instance) l) newEnv)]
                           [Local () (augmentEnv key (values (Local) l) newEnv)]
                           [Global () newEnv]
                           [NonLocal () (augmentEnv key (values (NonLocal) l) newEnv)]))]))
         (hash (list))
         (hash-keys env)))

;;addGlobalVars will use the vlist (list of variables and ScopeTypes) to insert the variables declared 
;;as Global in the environment
(define (addGlobalVars [env : Env]
                       [vlist : (listof (ScopeType * symbol))]) : Env
  (cond
    [(empty? vlist) env]
    [else (local [(define-values (t id) (first vlist))]
            (if (Global? t)
                (if (inEnv? id globalEnv)
                    (addGlobalVars (augmentEnv id (values (Global) (lookupEnv id globalEnv)) env) 
                                   (rest vlist))
                    (let ([newLocation (new-loc)])
                      (begin 
                        (set! globalEnv (augmentEnv id
                                                    (values (Global) newLocation)
                                                    env))
                        (addGlobalVars (augmentEnv id 
                                                   (values (Global) newLocation) 
                                                   env)
                                       (rest vlist)))))
                (addGlobalVars env (rest vlist))))]))

;;addNonLocals checks for errors that may be raised by the 'nonlocal' expression and,
;;if there are no errors, returns the same environment
(define (addNonLocals [env : Env]
                      [vlist : (listof (ScopeType * symbol))]) : Env
  (cond
    [(empty? vlist) env]
    [else (local [(define-values (t id) (first vlist))]
            (if (NonLocal? t)
                (if (inEnv? id env)
                    (if (Global? (getScopeType id env))
                        (error 'addNonLocals (string-append "no binding for nonlocal " (string-append (symbol->string id) " found")))
                        (addNonLocals env (rest vlist)))
                    (error 'addNonLocals (string-append "no binding for nonlocal " (string-append (symbol->string id) " found"))))
                (addNonLocals env (rest vlist))))]))

;;addLocals receives a list with Local variables candidates and a list with the variables that are 
;;already declared as global or nonlocal in this scope. It returns an environment with the 
;;appended correct Local variables
(define (addLocals [env : Env]
                   [localList : (listof (ScopeType * symbol))]
                   [othersList : (listof (ScopeType * symbol))]) : Env
  (cond
    [(empty? localList) env]
    [else (local [(define-values (t id) (first localList))]
            (if (foldl (lambda (l result) (or l result))
                       false
                       (map (lambda (st-id) (local [(define-values (t2 id2) st-id)]
                                              (if (equal? id id2)
                                                  true
                                                  false)))
                            othersList))
                (addLocals env (rest localList) othersList)
                (addLocals (augmentEnv id 
                                       (values (Local) (new-loc))
                                       env)
                           (rest localList) 
                           othersList)))]))

;; addInstanceVars takes an environment and a list of variables. 
;; it adds the instance variables to the env. 
;(define (addInstanceVars [env : Env]
;                   [localList : (listof (ScopeType * symbol))]
;                   [othersList : (listof (ScopeType * symbol))]) : Env
;  (cond
;    [(empty? localList) env]
;    [else (local [(define-values (t id) (first localList))]
;            (if (foldl (lambda (l result) (or l result))
;                       false
;                       (map (lambda (st-id) (local [(define-values (t2 id2) st-id)]
;                                              (if (equal? id id2)
;                                                  true
;                                                  false)))
;                            othersList))
;                (addInstanceVars env (rest localList) othersList)
;                (addInstanceVars (augmentEnv id 
;                                       (values (Instance) (new-loc))
;                                       env)
;                           (rest localList) 
;                           othersList)))]))

;;addArgs just appends the args to a list of (ScopeType * symbol), 
;;with the ScopeType 'Local'
(define (addArgs [lst : (listof (ScopeType * symbol))]
                 [args : (listof symbol)]) : (listof (ScopeType * symbol))
  (cond
    [(empty? args) lst]
    [else (addArgs (append (list (values (Local) (first args))) lst)
                   (rest args))]))

;;newEnvScope returns an environment with the changes needed for a new scope.
;;It basically changes the local tags to nonlocal ones.
(define (newEnvScope [env : Env]
                     [vlist : (listof (ScopeType * symbol))]
                     [args : (listof symbol)]
                     [vararg : symbol]) : Env
  (addLocals (addNonLocals (addGlobalVars (keepOldEnv env) 
                                          vlist)
                           vlist)
             (addArgs (filter (lambda (x) (local [(define-values (t id) x)]
                                            (if (Local? t)
                                                true
                                                false)))
                              vlist)
                      (if (equal? 'no-vararg vararg)
                          args
                          (append args (list vararg))))
             (filter (lambda (x) (local [(define-values (t id) x)]
                                   (if (Local? t)
                                       false
                                       true)))
                     vlist)))


;; version for classdefs...
#|
(define (newEnvScopeClass [env : Env]
                     [vlist : (listof (ScopeType * symbol))]
                     [args : (listof symbol)]
                     [vararg : symbol]) : Env
  (addInstanceVars (addLocals (addNonLocals (addGlobalVars (keepOldEnvClass env) 
                                          vlist)
                           vlist)
             (addArgs (filter (lambda (x) (local [(define-values (t id) x)]
                                            (if (Local? t)
                                                true
                                                false)))
                              vlist)
                      (if (equal? 'no-vararg vararg)
                          args
                          (append args (list vararg))))
             (filter (lambda (x) (local [(define-values (t id) x)]
                                   (if (Local? t)
                                       false
                                       true)))
                     vlist))
                   (addArgs (filter (lambda (x) (local [(define-values (t id) x)]
                                            (if (Instance? t)
                                                true
                                                false)))
                              vlist)
                      (if (equal? 'no-vararg vararg)
                          args
                          (append args (list vararg))))
                   (filter (lambda (x) (local [(define-values (t id) x)]
                                   (if (Instance? t)
                                       false
                                       true)))
                     vlist)))
|#

;;Adds a new identifier to our environment, with its location
(define (augmentEnv [id : symbol]
                    [sltuple : SLTuple]
                    [env : Env]) : Env
  (hash-set env id sltuple))

;;Adds a location and its Value to our Store
(define (augmentStore [location : Location]
                      [value : CVal]
                      [store : Store]) : Store
  (hash-set store location value))

;;inEnv? searches the environment for some identifier, returning true if
;;the identifier is already there and false otherwise
(define (inEnv? [id : symbol]
                [env : Env]) : boolean
  (type-case (optionof SLTuple) (hash-ref env id)
    [none () false]
    [some (v) true]))

;;getScopeType gets the ScopeType of 'id' in the environment 'env'
(define (getScopeType [id : symbol]
                      [env : Env]) : ScopeType
  (type-case (optionof SLTuple) (hash-ref env id)
    [none () (error 'getScopeType (string-append "Unbound Identifier : " (symbol->string id)))]
    [some (v) (local [(define-values (t l) v)]
                t)]))

;;lookupEnv searches the environment for some identifier
(define (lookupEnv [id : symbol]
                   [env : Env]) : Location
  (type-case (optionof SLTuple) (hash-ref env id)
    [none () (error 'lookupEnv (string-append "Unbound identifier error: " (symbol->string id)))]
    [some (v) (local [(define-values (t l) v)]
                ;(begin (display (string-append (string-append (symbol->string id) " ") (to-string l)))l))]))
                l)]))
                
;;lookupStore searches the store for an specific location
(define (lookupStore [loc : Location]
                     [store : Store]) : CVal
  (type-case (optionof CVal) (hash-ref store loc)
    [none () (error 'lookupStore (string-append (to-string loc) "Unbound location error."))]
    [some (v) (type-case CVal v
                [VUnbound () (error 'lookupStore "Unbound Identifier: using identifier before assignment")]
                [else v])]))

; lookupEnvStore merges functionality and allows exceptions to be thrown. 
(define (lookupEnvStore [id : symbol]
                        [env : Env]
                        [store : Store]) : AnswerC
  (type-case (optionof SLTuple) (hash-ref env id)
    [none () (interp-env (CApp (CId 'UnboundLocalError)
                               (list)
                               (list)
                               (CHash (hash (list)) (cType "list" (CId 'list)))) env store)]
    [some (loc) (type-case (optionof CVal) (hash-ref store (local [(define-values (t l) loc)] l))
                [none () (error 'lookupEnvStore "Something is missing from the store")]
                [some (v) (type-case CVal v
                            [VUnbound () (interp-env (CApp (CId 'UnboundLocalError)
                                                           (list)
                                                           (list)
                                                           (CHash (hash (list)) (cType "list" (CId 'list)))) env store)]
                            [else (ValueA v store)])])]))

;;lookupVar searches for the identifier first at the given environment, then at the globalEnv.
(define (lookupVar [id : symbol]
                   [env : Env]) : Location
  (type-case (optionof SLTuple) (hash-ref env id)
    [none () (lookupEnv id globalEnv)]
    [some (v) (local [(define-values (t l) v)]
                l)]))


;;puts the default args of the function in the places with CUnbounds
(define (add-default-args [args : (listof CVal)]
                          [defaults : (listof CVal)]) : (listof CVal)
  (cond
    [(empty? args) (list)]
    [else
     (if (equal? (last args) (VUnbound))
         (append (add-default-args (drop-right args 1) 
                                   (drop-right defaults 1)) 
                 (list (last defaults)))
         (append (add-default-args (drop-right args 1)
                                   defaults)
                 (list (last args))))]))



;; Should implement starargs...
;; requires VHash
(define (collapse-vhash-args [vhash : CVal] [n : number]) : (listof CVal)
  (cond
    [(<= (length (hash-keys (unbox (VHash-elts vhash)))) n) (list)]
    [else (type-case (optionof CVal) (hash-ref (unbox (VHash-elts vhash)) (VNum n))
            [some (s) (cons s (collapse-vhash-args vhash (+ n 1)))]
            [none () (error 'collapse-vhash-args "Missing element...")])]))

;; helper method for our interpreter
;; this interps each argument to a CVal and calls 
;; another helper that will put them in the env/store, before finally interpreting the body
(define (interp-args-CApp [body : CExp]
                          [env : Env]
                          [closEnv : Env]
                          [store : Store]
                          [argsIds : (listof symbol)]
                          [args : (listof CExp)]
                          [interpretedArgs : (listof CVal)]
                          [defargs : (listof CVal)]
                          ;[star : CExp]
                          ) : AnswerC
  (cond
    [(empty? args) 
     (interp-CApp body
                  (allocateLocals closEnv)
                  store
                  argsIds
                  (add-default-args (reverse interpretedArgs)
                                    defargs))]
    [else 
     (type-case AnswerC (interp-env (first args) env store)
       [ValueA (v s)
               (interp-args-CApp body
                                 env
                                 closEnv
                                 s
                                 argsIds
                                 (rest args)
                                 (cons v interpretedArgs)
                                 defargs
                                 ;star
                                 )]
       [BreakA (v s) (error 'interp-args-CApp "Why a break?")]
       [ContinueA (s) (error 'interp-args-CApp "Why a continue?")]
       [ExceptionA (v s) (ExceptionA v s)]
       [ReturnA (v s) (ReturnA v s)])]))

;;helper method that allocates a new position for all of the local variables in the environment. Used when applying a function, because
;;each time we apply we are using new arguments/locals, not the old ones.
(define (allocateLocals [env : Env]) : Env
  (foldl (lambda (key newEnv) 
           (type-case (optionof SLTuple) (hash-ref env key)
             [none () (error 'allocateLocals "Cannot find key inside hash with this key in hash-keys: something is very wrong")]
             [some (v) (local [(define-values (t l) v)]
                         (type-case ScopeType t
                           [Local () (augmentEnv key (values t (new-loc)) newEnv)]
                           [else (augmentEnv key v newEnv)]))]))
         (hash (list))
         (hash-keys env)))

;;puts all the identifiers and values in the environment and the store,
;;and applies the body of the closure
(define (interp-CApp [body : CExp]
                     [closEnv : Env]
                     [store : Store]
                     [argsIds : (listof symbol)]
                     [args : (listof CVal)]) : AnswerC
  (cond
    [(not (equal? (length argsIds) (length args)))
     (interp-env (CError (Make-throw 'TypeError "(interp-CApp) Arity mismatch"))
                 env
                 store)]
    
    [(empty? args) (let ([_newLoc (new-loc)])
                     (let ([_newEnv (augmentEnv 'locals (values (NonLocal) _newLoc) closEnv)]) 
                       (type-case AnswerC (interp-env body 
                                                      _newEnv
                                                      (augmentStore _newLoc 
                                                                    (VClosure (newEnvScope _newEnv (list) (list) 'no-vararg) 
                                                                              (list) 'no-vararg (CPrim1 '_locals (CHolder (VSymbolList (getLocals _newEnv)))) (list) (new-uid) false)
                                                                    store))
                         [ValueA (v s) (ValueA v s)]
                         [BreakA (v s) (error 'interp-CApp "Should not be a break in function call")]
                         [ContinueA (s) (error 'interp-CApp "Should not be a continue in function call")]
                         [ExceptionA (v s) (ExceptionA v s)]
                         [ReturnA (v s) (ValueA v s)])
                       ))]
    [else 
     (interp-CApp body
                  closEnv
                  (augmentStore (lookupEnv (first argsIds) closEnv)
                                (first args)
                                store)
                  (rest argsIds)
                  (rest args))]))

;; tagof wrapper
(define (interp-tagof [arg : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env arg env store)
    [ValueA (v s) (ValueA (VStr (get-tag v)) s)]
    [BreakA (v s) (error 'interp-tagof "Should not be a break in input to tagof")]
    [ContinueA (s) (error 'interp-tagof "Should not be a continue in input to tagof")]
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v s) (ReturnA v s)]))


;; This is the tagof operator that we will need later...
(define (get-tag [val : CVal]) : string
  (type-case CVal val
    [VNum (n) ;"int"] ;; this really should distinguish ints from floats...
          (cond
            [(fixnum? n) "int"]
            [(flonum? n) "float"])]
    [VStr (s) "string"]
    [VClosure (e a varg b defargs uid classmethod) "function"]
    [VTrue () "bool"]
    [VFalse () "bool"]
    [VNone () "NoneType"] ;; TODO this looks like a class name. Maybe we should make it so?
    [VPass () "pass"] ;; should never be reached. 
    [VUnbound () "unbound"]
    
    [VHash (elts uid type) 
           (Type-name type)]
    [VSymbolList (lst) "no one should be getting the tag of this thing, seriously"]
    
    ))


;; This is going to be an interp function that works on arbitrary CExps.
;; interp-binop
(define (interp-binop [op : symbol] [e1 : CExp] [e2 : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env e1 env store)
    [ValueA (v1 s1) (type-case AnswerC (interp-env e2 env s1)
                      [ValueA (v2 s2) (ValueA (case op
                                                ['has-field (type-case CVal v1
                                                              [VHash (elts uid t) (try (begin (getAttr v2 v1 env store)
                                                                                              (VTrue))
                                                                                       (lambda () (VFalse)))]
                                                              [else (VFalse)])]
                                                [else (handle-op op v1 v2)]) s2)]
                      [BreakA (v2 s2) (error 'interp-binop "Why is there a break in second arg?")]
                      [ContinueA (s) (error 'interp-binop "Why is there a continue in second arg?")]
                      [ExceptionA (v s) (ExceptionA v s)]
                      [ReturnA (v2 s2) (ReturnA v2 s2)])]
    [BreakA (v2 s2) (error 'interp-binop "Why is there a break in first arg?")]
    [ContinueA (s) (error 'interp-binop "Why is there a continue in first arg?")]
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v1 s1) (ReturnA v1 s1)]))


(define (duplicate-string [str : string] [num : number]) : string
  (cond
    [(flonum? num) (error 'duplicate-string "does not work with floats...")]
    [(<= num 0) ""]
    [else (string-append str (duplicate-string str (- num 1)))]))

;; just a helper...
(define (repeat-list [lst : (listof 'a)] [n : number]) : (listof 'a)
  (cond
    [(= n 0) empty]
    [else (append lst (repeat-list lst (- n 1)))]))


(define (duplicate-tuple [tup : (hashof CVal CVal)] [n : number]) : (hashof CVal CVal)
  (cond
    [(flonum? n) (error 'duplicate-tuple "does not work with floats...")]
    [(<= n 0) (hash (list))]
    [else (make-new-map (map (lambda (x) (VNum x)) 
                             (range (* n 
                                       ;(argmax (lambda (x) x) (map (lambda (x) (VNum-n x)) 
                                       (type-case (optionof CVal) 
                                         (hash-ref tup (VStr "__size__"))
                                         [some (s) (VNum-n s)]
                                         [none () (error 'duple "no size field")]))))
                        (repeat-list (hash-values tup) n))]))

(define (cval-range [n : number]) : (listof CVal)
  (map (lambda (x) (VNum x)) (range n)))

;; Assumes we are dealing with VHashs
(define (merge-python-lists (l1 : CVal) (l2 : CVal)) : (hashof CVal CVal)
  (local ([define h1 (unbox (VHash-elts l1))]
          [define h2 (unbox (VHash-elts l2))]
          [define len2 (type-case (optionof CVal) (hash-ref h2 (VStr "__size__"))
                         [some (s) (VNum-n s)]
                         [none () (error 'merge-python-lists (string-append "?? "
                                                                            (to-string h2)))])]
          [define keylenh (type-case (optionof CVal) (hash-ref h1 (VStr "__size__"))
                            [some (s) (VNum-n s)]
                            [none () (error 'merge-python-lists "???1")])])
    (hash-set (foldl (lambda (x h) (hash-set h (VNum (+ (VNum-n x) keylenh)) 
                                             (type-case (optionof CVal) (hash-ref h2 x)
                                               [some (s) s]
                                               [none () (error 'merge-python-lists "???")])))
                     h1
                     (cval-range len2)) 
              (VStr "__size__") 
              (VNum (+ keylenh len2)))))

(define (fold-equality [h1 : (hashof CVal CVal)] [h2 : (hashof CVal CVal)]) : boolean
  (cond
    [(not (= (length (hash-keys h1)) (length (hash-keys h2)))) #f]
    [else (foldl (lambda (x y) (and y (type-case (optionof CVal) (hash-ref h1 x)
                                        [some (s1) (type-case (optionof CVal) (fold-eq-contains (hash-keys h2) x)
                                                     [some (vk) (type-case (optionof CVal) (hash-ref h2 vk)
                                                                  [some (s2) (check-equality s1 s2)]
                                                                  [none () #f])]
                                                     [none () #f])]
                                        [none () (error 'fold-equality "something is terribly wrong here...")]))) 
                 #t 
                 (hash-keys h1))]))

(define (fold-eq-contains [lst : (listof CVal)] [val : CVal]) : (optionof CVal)
  (cond
    [(empty? lst) (none)]
    [(check-equality val (first lst)) (some (first lst))]
    [else (fold-eq-contains (rest lst) val)]))


;; check-equality
(define (check-equality [v1 : CVal] [v2 : CVal]) : boolean
  (type-case CVal v1
    [VHash (elts1 uid1 type1) 
           (type-case CVal v2
             [VHash (elts2 uid2 type2) (if (equal? (Type-name type1) (Type-name type2))
                                           (fold-equality (unbox elts1) (unbox elts2))
                                           false)]
             [else false])]
    [else (equal? v1 v2)]))

;; this function handles binary operations
;; it does NO TYPE CHECKING! We will need to check types in library functions. 
; another case for the library functions: where else do we put the errors? 
;; if we have regular exceptions, we will need to throw them higher up...
;; Need a "tagof" unary operator. Doesn't python have "type"?

;; We need separate float and intger values. 


;; Also, this function should be in the "primitives" file. 
(define (handle-op [op : symbol] [v1 : CVal] [v2 : CVal]) : CVal
  (case op
    ['eq (if (check-equality v1 v2) (VTrue) (VFalse))]
    ['notEq (if (check-equality v1 v2) (VFalse) (VTrue))]
    ['num+ (VNum (+ (VNum-n v1) (VNum-n v2)))]
    ['string+ (VStr (string-append (VStr-s v1) (VStr-s v2)))]
    ['num- (VNum (- (VNum-n v1) (VNum-n v2)))]
    ['num* (VNum (* (VNum-n v1) (VNum-n v2)))]
    ['num/ (VNum (/ (VNum-n v1) (VNum-n v2)))]
    ['num% (VNum (modulo (VNum-n v1) (VNum-n v2)))]
    ['num-lt (if (< (VNum-n v1) (VNum-n v2)) (VTrue) (VFalse))]
    ['string-lt (if (string<? (VStr-s v1) (VStr-s v2)) (VTrue) (VFalse))]
    ['num-lte (if (<= (VNum-n v1) (VNum-n v2)) (VTrue) (VFalse))]
    ['string-lte (if (string<=? (VStr-s v1) (VStr-s v2)) (VTrue) (VFalse))]
    ['num-gt (if (> (VNum-n v1) (VNum-n v2)) (VTrue) (VFalse))]
    ['string-gt (if (string>? (VStr-s v1) (VStr-s v2)) (VTrue) (VFalse))]
    ['num-gte (if (>= (VNum-n v1) (VNum-n v2)) (VTrue) (VFalse))]
    ['string-gte (if (string>=? (VStr-s v1) (VStr-s v2)) (VTrue) (VFalse))]
    ['duplicate (VStr (duplicate-string (VStr-s v1) (VNum-n v2)))] ;; throws exception if types are wrong.
    ['duple (VHash (box (duplicate-tuple (unbox (VHash-elts v1)) (VNum-n v2))) (new-uid) (VHash-type v1))]
    [else (error op "handle-op: case not implemented")]))



;;or returns e1 if its value is truthy; if not, 
;;returns e2's value
(define (interp-or [e1 : CExp]
                   [e2 : CExp]
                   [env : Env]
                   [store : Store]) : AnswerC
  (type-case AnswerC (interp-env e1 env store)
    [ValueA (v s) (if (isTruthy v)
                      (ValueA v s)
                      (interp-env e2 env s))]
    [BreakA (v s) (error 'interp-or "Break!")]
    [ContinueA (s) (error 'interp-or "Continue!")]
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v s) (ReturnA v s)]))

;;and returns e1 if its value is not truthy; else, 
;;returns e2's value
(define (interp-and [e1 : CExp]
                    [e2 : CExp]
                    [env : Env]
                    [store : Store]) : AnswerC
  (type-case AnswerC (interp-env e1 env store)
    [ValueA (v s) (if (not (isTruthy v))
                      (ValueA v s)
                      (interp-env e2 env s))]
    [BreakA (v s) (error 'interp-and "Break!")]
    [ContinueA (s) (error 'interp-and "Continue!")]
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v s) (ReturnA v s)]))

;;  get-uid returns the uid for any type that has one
(define (get-uid [v : CVal]) : Uid
  (type-case CVal v
    [VClosure (e a varg b defargs uid classmethod) uid]
    [VHash (elts uid type) uid]
    [else (error 'get-uid "should not use get-uid for types that do not have Uid's")]))

;;is returns true if e1 and e2 are the same object in python
(define (interp-is [e1 : CExp]
                   [e2 : CExp]
                   [env : Env]
                   [store : Store]) : AnswerC
  (type-case AnswerC (interp-env e1 env store)
    [ValueA (v1 s1)
            (type-case AnswerC (interp-env e2 env s1)
              [ValueA (v2 s2)
                      (type-case CVal v1
                        [VNum (n1) (type-case CVal v2
                                     [VNum (n2) (if (equal? n1 n2)
                                                    (ValueA (VTrue) s2)
                                                    (ValueA (VFalse) s2))]
                                     [else (ValueA (VFalse) s2)])]
                        [VNone () (type-case CVal v2
                                    [VNone () (ValueA (VTrue) s2)]
                                    [else (ValueA (VFalse) s2)])]
                        [VTrue () (type-case CVal v2
                                    [VTrue () (ValueA (VTrue) s2)]
                                    [else (ValueA (VFalse) s2)])]
                        [VFalse () (type-case CVal v2
                                     [VFalse () (ValueA (VTrue) s2)]
                                     [else (ValueA (VFalse) s2)])]
                        [VStr (str1) (if (equal? v1 v2)
                                         (ValueA (VTrue) s2)
                                         (ValueA (VFalse) s2))]
                        [else (try (if (equal? (get-uid v1) (get-uid v2))
                                       (ValueA (VTrue) s2)
                                       (ValueA (VFalse) s2))
                                   (lambda () (ValueA (VFalse) s2)))])]
              [BreakA (v s) (error 'interp-is "Break!")]
              [ContinueA (s) (error 'interp-is "Continue!")]
              [ExceptionA (v s) (ExceptionA v s)]
              [ReturnA (v2 s2) (ReturnA v2 s2)])]
    [BreakA (v s) (error 'interp-is "Break!")]
    [ContinueA (s) (error 'interp-is "Continue!")]
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v1 s1) (ReturnA v1 s1)]))


;; 
(define (hash-values [h : (hashof 'a 'b)]) : (listof 'b)
  (map (lambda (x) (type-case (optionof 'b) (hash-ref h x)
                     [none () (error 'hash-values "This exists...")]
                     [some (v) v])) (hash-keys h)))

;; convenience function
(define (is-substring (str1 : string) (str2 : string) (index : number)) : boolean
  (cond
    [(>= (+ index (string-length str1)) (string-length str2)) (equal? (substring str2 index (+ (string-length str1) index)) str1)]
    [else (if (equal? (substring str2 index (+ (string-length str1) index)) str1)
              #t
              (is-substring str1 str2 (+ index 1)))]))


;; interp-in
(define (interp-in (left : CExp) (right : CExp) (env : Env) (store : Store)) : AnswerC
  (type-case AnswerC (interp-env left env store)
    [ValueA (v1 s1)
            (type-case AnswerC (interp-env right env s1)
              [ValueA (v2 s2)
                      (type-case CVal v2
                        [VStr (str2) (type-case CVal v1
                                       [VStr (str1) (if (is-substring str1 str2 0) ;;
                                                        (ValueA (VTrue) s2)  
                                                        (ValueA (VFalse) s2))]
                                       [else (error 'interp-in "\"in\" not valid for these (differing?) types")])]
                        [VHash (elts-box uid type) 
                               (cond 
                                 [(equal? (Type-name type) "_dict") (if (member v1 (hash-keys (unbox elts-box)))
                                                                       (ValueA (VTrue) s2)
                                                                       (ValueA (VFalse) s2))]
                                 [(or (equal? (Type-name type) "list")
                                      (equal? (Type-name type) "tuple")
                                      (equal? (Type-name type) "set")) (if (member v1 (hash-values (unbox elts-box)))
                                                                             (ValueA (VTrue) s2)
                                                                             (ValueA (VFalse) s2))])]
                        [else (error 'interp-in "\"in\" is not valid for arguments of this type (yet?)")])]
              [BreakA (v s) (error 'interp-in "Break!")]
              [ContinueA (s) (error 'interp-in "Continue!")]
              [ExceptionA (v s) (ExceptionA v s)]
              [ReturnA (v2 s2) (ReturnA v2 s2)])]
    [BreakA (v s) (error 'interp-in "Break!")]
    [ContinueA (s) (error 'interp-in "Continue!")]
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v1 s1) (ReturnA v1 s1)]))

;; handles merging of list and tuple types
(define (merge-listy-things (e1 : CExp) (e2 : CExp) (env : Env) (store : Store)) : AnswerC ;; TODO not sure we need this function. But we can get rid of it later. 
  (type-case AnswerC (interp-env e1 env store)
    [ValueA (v1 s1) (type-case AnswerC (interp-env e2 env s1)
                      [ValueA (v2 s2) (type-case CVal v1
                                        [VHash (elts-box1 uid1 type1) (type-case CVal v2
                                                 [VHash (elts-box2 uid2 type2) (ValueA (VHash (box (merge-python-lists v1 v2)) (new-uid) type1) s2)]
                                                 [else (error 'merge-listy-things "This also should never happen. ")])]
                                        [else (error 'merge-listy-things (string-append "This should never happen. "
                                                                                        (to-string v1)))])]
                      [BreakA (v s) (error 'merge-listy-things "Break!")]
                      [ContinueA (s) (error 'merge-listy-things "Continue!")]
                      [ExceptionA (v2 s2) (ExceptionA v2 s2)]
                      [ReturnA (v2 s2) (ReturnA v2 s2)])]
    [BreakA (v s) (error 'merge-listy-things "Break!")]
    [ContinueA (s) (error 'merge-listy-things "Continue!")]
    [ExceptionA (v1 s1) (ExceptionA v1 s1)]
    [ReturnA (v1 s1) (ReturnA v1 s1)]))



;; isTruthy returns false if the CVal value is False to python
;; and true otherwise
(define (isTruthy [value : CVal]) : boolean
  ;;We need to finish this ----------
  (type-case CVal value
    [VTrue () true]
    [VNum (n)
          (if (= n 0)
              false
              true)]
    [VStr (s)
          (if (> (string-length s) 0)
              true
              false)]
    [VHash (elts uid t)
           (cond
             [(and (or (equal? (Type-name t) "list")
                       (or (equal? (Type-name t) "tuple")
                           (equal? (Type-name t) "_dict"))) (type-case (optionof CVal) (hash-ref (unbox elts) (VStr "__size__"))
                                                             [none () (error 'isTruthy (string-append "Does not have size field:"
                                                                                                      (to-string value)))]
                                                             [some (s) (= (VNum-n s) 0)])) false]
             [else true])]
    [VClosure (e a varg body defargs uid classmethod) true]
    [else false]))


(define (make-new-map [keys : (listof CVal)] [vals : (listof CVal)]) : (hashof CVal CVal)
  (cond 
    [(empty? keys) (hash (list))]
    [(cons? keys) (hash-set (make-new-map (rest keys) (rest vals)) (first keys) (first vals))]))

;; change-string-to-list
;; converts a string into a list
(define (change-string-to-list (s : string)) : (hashof CVal CVal)
  (make-new-map (map (lambda (x) (VNum x)) (range (string-length s))) 
                (map (lambda (x) (VStr (list->string (list x)))) (string->list s))))


;; adds an element to set's hash map only if it isn't already there
(define (set-no-duplicates [lst : (listof CVal)]) : (hashof CVal CVal)
  (cond
    [(empty? lst) (hash (list))]
    [else (local [(define rec (set-no-duplicates (rest lst)))]
            (if (foldl (lambda (x y) (or y (check-equality x (first lst)))) #f (hash-values rec))
                rec
                (hash-set (set-no-duplicates (rest lst)) (first lst) (first lst))))]))
    


;; handle unary operations - akin to handle-op
(define (handle-unary [prim : symbol] [arg : CVal] [env : Env] [store : Store]) : CVal
  (case prim
    ['print (begin (display (string-append (pretty arg) "\n")) arg)]
    ['not (if (isTruthy arg) (VFalse) (VTrue))]
    ['negative (type-case CVal arg
                 [VNum (n) (VNum (- 0 n))] ;; gotta be a better way...
                 [else (error 'interp "Tried to negate a non-number")])] ;; TODO handle errors outside...
    ['invert (type-case CVal arg
               [VNum (n) (VNum (bitwise-not n))]
               [else (error 'handle-unary "Tried to invert a non-number")])]
    ['tagof (VStr (get-tag arg))]
    ['length (type-case CVal arg
               [VStr (str) (VNum (string-length str))]
               [VHash (elts uid t) (VNum (length (hash-keys (unbox elts))))]
               [else (error 'interp-length "Should only be called on strings, lists, dicts and tuples.")])]
    ['to-bool (if (isTruthy arg) (VTrue) (VFalse))]
    ['to-string (VStr (pretty arg))]
    ['to-float (type-case CVal arg
                 [VFalse () (VNum 0.0)]
                 [VTrue () (VNum 1.0)]
                 [VNum (n) (VNum (+ 0.0 n))]
                 [VStr (s) (error 'interp-to-num "String to Num not implemented yet.")] ;; TODO handle outside...
                 [else (error 'interp-to-num "Should not be called on this type.")])]
    ['to-int (type-case CVal arg
               [VFalse () (VNum 0)]
               [VTrue () (VNum 1)]
               [VNum (n) (VNum n)] ;; TODO figure this out...
               [VStr (s) (error 'interp-to-num "String to Num not implemented yet.")]
               [else (error 'interp-to-num "Should not be called on this type.")])]
    ['to-list (type-case CVal arg
                [VHash (elts uid type) (cond 
                                         [(or (equal? (get-tag arg) "list") (equal? (get-tag arg) "tuple"))
                                           ;(if (or (isInstanceOf arg (Type "list" (CNone))) (isInstanceOf arg (Type "tuple" (CNone))))
                                           (VHash (box (hash-set (unbox elts) 
                                                                 (VStr "__size__") 
                                                                 (type-case (optionof CVal) (hash-ref (unbox elts) (VStr "__size__"))
                                                                   [some (s) s]
                                                                   [none () (error 'interp-to-list "no __size__ field")]))) 
                                                  (new-uid) 
                                                  (transform-ctype (cType "list" (CId 'list)) env store))]
                                         [(equal? (get-tag arg) "set") 
                                          (VHash (box (hash-set (make-new-map (vnum-range (+ 0 (length (hash-keys (unbox elts)))))
                                                                              (hash-values (unbox elts)))
                                                                (VStr "__size__")
                                                                (VNum (length (hash-keys (unbox elts))))))
                                                 (new-uid)
                                                 (transform-ctype (cType "list" (CId 'list)) env store))]
                                           [else (error 'interp-to-list "arguments of this type are not supported")])]
                [VStr (s) (VHash (box (hash-set (change-string-to-list s) (VStr "__size__") (VNum (string-length s)))) 
                                 (new-uid) 
                                 (transform-ctype (cType "list" (CId 'list)) env store))] ;; string to list
                [else (error 'interp-to-list "Unsupported Type")])]
    ['to-tuple (type-case CVal arg
                 [VHash (elts uid type) (if (or (equal? (get-tag arg) "list") (equal? (get-tag arg) "tuple"))
                                            ;(if (or (isInstanceOf arg (Type "list" (CNone))) (isInstanceOf arg (Type "tuple" (CNone))))
                                            (VHash (box (hash-set (unbox elts) 
                                                                  (VStr "__size__") 
                                                                  (type-case (optionof CVal) (hash-ref (unbox elts) (VStr "__size__"))
                                                                    [some (s) s]
                                                                    [none () (error 'interp-to-tuple "no __size__ field")])))
                                                   (new-uid) 
                                                   (transform-ctype (cType "tuple" (CId 'tuple)) env store))
                                            (error 'interp-to-tuple "arguments of this type are not supported"))]
                 [VStr (s) (VHash (box (hash-set (change-string-to-list s) (VStr "__size__") (VNum (string-length s)))) 
                                  (new-uid) 
                                  (transform-ctype (cType "tuple" (CId 'tuple)) env store))] ;; string to list
                 [else (error 'interp-to-list "Unsupported Type")])]
    ['to-set (type-case CVal arg
               [VHash (elts uid type) (if (or (equal? (get-tag arg) "list") (equal? (get-tag arg) "tuple"))
                                          (VHash (box (local [(define sz (type-case (optionof CVal) (hash-ref (unbox elts) (VStr "__size__"))
                                                                           [some (s) (VNum-n s)] ;; TODO could be unsafe...
                                                                           [none () (error 'interp-to-set "List without a size field")]))] ;; TODO don't add duplicates by check-equality!
                                                        (set-no-duplicates (map (lambda (x) (local [(define v (type-case (optionof CVal) (hash-ref (unbox elts) x)
                                                                                                                [some (sv) sv]
                                                                                                                [none () (error 'interp-to-set "List or tuple with a missing element")]))]
                                                                                              v)) (vnum-range sz)))))
                                                 (new-uid)
                                                 (transform-ctype (cType "set" (CId 'set)) env store))
                                          (error 'interp-to-set "Arguments of this type aren't supported here. "))]
               [VStr (s) (error 'interp-to-set "We weren't expecting to need this case...")]
               [else (error 'interp-to-set "Unsupported Type")])]
    ['_locals (make-localsfunc-dict arg env store)];(lookupStore (lookupVar 'varLocals env) store)]
    ['_localsClass (make-localsclass-list arg env store)]
    ['_super (Type-baseType (VHash-type (lookupStore (lookupVar (string->symbol (VStr-s arg)) env) store)))]
    [else (error prim "handle-unary: Case not handled yet")]))

;; wrapper around unary operations
(define (interp-unary [prim : symbol] [arg : CExp] [env : Env] [store : Store]) : AnswerC
  (type-case AnswerC (interp-env arg env store)
    [ValueA (v s) (ValueA (handle-unary prim v env s) s)]
    [BreakA (v s) (error 'interp-unary "Break!")]
    [ContinueA (s) (error 'interp-unary "Continue!")]
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v s) (ReturnA v s)]))

;;interps every object represented as a hash
(define (interp-CHash [keys : (listof CExp)]
                      [h : (hashof CExp CExp)]
                      [type : VType]
                      [env : Env]
                      [store : Store]) : AnswerC 
  (cond
    [(empty? keys) (ValueA (VHash (box (hash (list))) (new-uid) type) store)]
    [(cons? keys) 
     (type-case AnswerC (interp-env (first keys) env store)
       [ValueA (v1 s1)
               (type-case (optionof CExp) (hash-ref h (first keys))
                 [none () (error 'interp-CHash "Cannot find key inside hash with this key: something is very wrong")]
                 [some (value) 
                       (type-case AnswerC (interp-env value env s1)
                         [ValueA (v2 s2) (type-case AnswerC (interp-CHash (rest keys) h type env s2)
                                           [ValueA (v3 s3) (ValueA (VHash (begin (set-box! (VHash-elts v3) (hash-set (unbox (VHash-elts v3)) v1 v2)) (VHash-elts v3)) 
                                                                          (VHash-uid v3) 
                                                                          type) 
                                                                   s3)]
                                           [BreakA (v s) (error 'interp-CHash "Break!")]
                                           [ContinueA (s) (error 'interp-CHash "Continue!")]
                                           [ExceptionA (v3 s3) (ExceptionA v3 s3)]
                                           [ReturnA (v3 s3) (error 'interp-CHash "Shouldn't see a return here...")])]
                         [BreakA (v s) (error 'interp-CHash "Break!")]
                         [ContinueA (s) (error 'interp-CHash "Continue!")]
                         [ExceptionA (v2 s2) (ExceptionA v2 s2)]
                         [ReturnA (v2 s2) (error 'interp-CHash "This should never be a return.")])])]
       [BreakA (v s) (error 'interp-CHash "Break!")]
       [ContinueA (s) (error 'interp-CHash "Continue!")]
       [ExceptionA (v1 s1) (ExceptionA v1 s1)]
       [ReturnA (v1 s1) (error 'interp-CHash "This should never be a return!!")])]))


;; TODO somehow we need to have a function to convert a CId into a string
(define (id-to-string (id : CExp)) : string
  (type-case CExp id
    [CId (s) (symbol->string s)]
    [else (error 'id-to-string "Not an ID")]))

(define (get-typesList [obj : CVal]) : (listof string)
  (type-case CVal obj
    [VHash (elts uid type) 
           (if (not (or (equal? (Type-name type) "class") (equal? (Type-name type) "primitive-class")))
               (append (list (Type-name type))
                       (get-typesList (Type-baseType type)))
               (type-case (optionof CVal) (hash-ref (unbox elts) (VStr "__name__"))
                 [some (v) (append (list (VStr-s v))
                                   (type-case CVal (Type-baseType type)
                                     [VNone () (list)]
                                     [else (get-typesList (Type-baseType type))]))]
                 [none () (error 'get-typesList "a class should have a name")]))]
    [else (list (get-tag obj))]))

;; isInstanceOf checks whether 'obj' is of the same type or of one of the base types of 'type'
(define (isInstanceOf [obj : CVal]
                      [type : string]
                      [env : Env]
                      [store : Store]) : boolean
  (member type (get-typesList obj)))


;; wrapper for isInstanceOf
(define (interp-isinstance [obj : CExp]
                           [type : CExp]
                           [env : Env]
                           [store : Store]) : AnswerC
  (type-case AnswerC (interp-env obj env store)
    [ValueA (v1 s1) (type-case AnswerC (interp-env type env s1) ;; This takes a string. Do we want to update it to take a class?
                      [ValueA (v2 s2) (type-case CVal v2        ;; I think we probably should update it. But we can do that later. 
                                        [VStr (ty) (if (isInstanceOf v1 ty env s2)
                                                       (ValueA (VTrue) s2)
                                                       (ValueA (VFalse) s2))]
                                        [else (error 'interp-isinstance "This error should not appear.")])]
                      [BreakA (v s) (error 'interp-isi-instance "Break!")]
                      [ContinueA (s) (error 'interp-is-instance "Continue!")]
                      [ExceptionA (v s) (ExceptionA v s)]
                      [ReturnA (v s) (ReturnA v s)])]
    [BreakA (v s) (error 'interp-is-instance "Break!")]
    [ContinueA (s) (error 'interp-is-instance "Continue!")]
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v s) (ReturnA v s)]))
                           
;; TODO This needs to be adapted to work with integers and other primitive types as well...

;; hasMatchingException checks whether any of the except clauses deal with the raised object
;; we assume that the CExcHandler-type is always a CId
(define (hasMatchingException [exc : CVal] 
                              [handlers : (listof CExceptionHandler)]
                              [env : Env]
                              [store : Store]) : boolean
  (cond
    [(empty? handlers) false]
    [(cons? handlers) 
     (if (CNone? (CExcHandler-type (first handlers)))
         true
         (if (isInstanceOf exc 
                           (type-case CExp (CExcHandler-type (first handlers))
                             [CId (x) (symbol->string x)]
                             [else (error 'hasMatchingException "some CExcHandler-type is not an ID")])
                           env 
                           store)
             true
             (hasMatchingException exc (rest handlers) env store)))]))


(define (interp-handlers [handlers : (listof CExceptionHandler)] [val : CVal] [env : Env] [store : Store]) : AnswerC 
  (cond
    [(empty? handlers) (ExceptionA val store)]
    [(cons? handlers) (if (equal? (CExcHandler-name (first handlers)) 'no-name)
                          (interp-env (CExcHandler-body (first handlers)) env store)
                          (local [(define location (new-loc))]
                            (interp-env (CExcHandler-body (first handlers)) 
                                      (augmentEnv (CExcHandler-name (first handlers)) (values (Local) location) env) 
                                      (augmentStore location val store))))]))


;; getAttr gets the attribute from an object (VHash) -----------------------------------------------------------throw an exception instead of an error
(define (getAttr [attr : CVal]
                 [obj : CVal]
                 [env : Env]
                 [store : Store]) : CVal
  (type-case CVal obj
    [VHash (elts uid type) (type-case (optionof CVal) (hash-ref (unbox elts) attr)
                             ;;if the object has the attribute we're looking for
                             [some (v) v]
                             ;;else, we search in its super/base class
                             [none () (getAttr attr
                                               ;#|
                                               (type-case CVal (Type-baseType type)
                                                 [VNone () (error 'getAttr (string-append "non-existent attribute, Unbound Identifier: " (to-string attr)))]
                                                 [VHash (elts uid t) (Type-baseType type)]
                                                 [else (error 'getAttr "a class has something other than a VNone or a VHash as its baseType")])
                                               env
                                               store)])]
    
    [else (error 'getAttr "tried to get attribute from non-object")]))


(define (interp-func [args : (listof symbol)]
                     [vararg : symbol]
                     [body : CExp] 
                     [vlist : (listof (ScopeType * symbol))]
                     [defargs : (listof CExp)] 
                     [defvals : (listof CVal)]
                     [classmethod : boolean]
                     [env : Env]
                     [store : Store]) : AnswerC
  (cond 
    [(empty? defargs) 
     (ValueA (VClosure (newEnvScope env vlist args vararg) 
                       args 
                       vararg
                       body 
                       (reverse defvals) 
                       (new-uid)
                       classmethod) 
             store)]
    [else (type-case AnswerC (interp-env (first defargs) env store)
            [ValueA (v s) (interp-func args vararg body vlist (rest defargs) (cons v defvals) classmethod env s)]
            [BreakA (v s) (error 'interp-func "Break!")]
            [ContinueA (s) (error 'interp-func "Continue!")]
            [ExceptionA (v s) (ExceptionA v s)]
            [ReturnA (v s) (error 'interp-func "I don't think this should even happen.")])]))

;;creates a hash with all of the positional arguments
(define (group-positional-arguments [ids : (listof symbol)]
                                    [args : (listof CExp)]) : (hashof symbol CExp)
  (cond
    [(or (empty? args) (empty? ids))
     (hash (list))]
    [else
     (hash-set (group-positional-arguments (rest ids)
                                           (rest args))
               (first ids)
               (first args))]))

;;adds the keyword arguments to the hash of arguments, and throws an error if we have one
(define (group-keyword-arguments [ids : (listof symbol)]
                                 [keywargs : (listof (symbol * CExp))]
                                 [argList : (hashof symbol CExp)]) : (hashof symbol CExp)
  (cond
    [(empty? keywargs) argList]
    [else
     (local [(define-values (id exp) (first keywargs))]
       (if (member id ids)
           (if (member id (hash-keys argList))
               (error 'group-keyword-arguments "keyword argument for argument already defined by positional argument: throw TypeError exception")
               (group-keyword-arguments ids 
                                        (rest keywargs)
                                        (hash-set argList id exp)))
           (error 'group-keyword-arguments "non-existing keyword argument: throw a TypeError exception.")))]))

;;creates an ordered list of arguments with CUnbound representing the default arguments in the position of missing arguments.
(define (group-all-arguments [ids : (listof symbol)]
                             [argList : (hashof symbol CExp)]
                             [varargid : symbol]
                             [varargs : (listof CExp)]) : (listof CExp)
  (let [(arguments (foldl (lambda (id lst) (type-case (optionof CExp) (hash-ref argList id)
                                             [none () (append lst (list (CUnbound)))]
                                             [some (v) (append lst (list v))]))
                          (list)
                          ids))]
    (if (not (equal? varargid 'no-vararg))
        (append arguments (list (create-clist varargs)))
        arguments)))

;;checks if the list of arguments has some CUnbound in a position that is not from some default argument
(define (group-check-defaults [ids : (listof symbol)]
                              [nDefArgs : number]
                              [argsList : (listof CExp)]) : boolean
  (if (member (CUnbound) (drop-right argsList nDefArgs))
      false
      true))
      

;; group-arguments create a list of arguments in the correct order from the list of positional args, keyword args and the number of existing default args.
;; if varargid is different from 'no-vararg, adds a new argument to the list of arguments, that is a list containing the positional arguments in excess
(define (group-arguments [ids : (listof symbol)]
                         [varargid : symbol]
                         [args : (listof CExp)]
                         [keywargs : (listof (symbol * CExp))]
                         [nDefArgs : number]
                         [argList : (hashof symbol CExp)]
                         [varargs : (listof CExp)]) : (listof CExp)
  (cond
    [(and (< (length ids) (length args)) (equal? varargid 'no-vararg))
     (error 'group-arguments "arity mismatch: more arguments than function can handle. Should throw TypeError exception.")]
    [(not (empty? args))
     (group-arguments ids
                      varargid
                      (list)
                      keywargs
                      nDefArgs
                      (group-positional-arguments ids args)
                      (if (< (length ids) (length args))
                          (list-tail args (length ids))
                          (list)))]; (CHash (hash (list)) (Type "list" (list))))))]
    [(not (empty? keywargs))
     (group-arguments ids
                      varargid
                      (list)
                      (list)
                      nDefArgs
                      (group-keyword-arguments ids keywargs argList)
                      varargs)]
    [else
     (let [(groupedArgs (group-all-arguments ids
                                             argList
                                             varargid
                                             varargs))]
       (if (group-check-defaults ids
                                 nDefArgs
                                 (if (equal? varargid 'no-vararg)
                                     groupedArgs
                                     (drop-right groupedArgs 1)))
           groupedArgs
           (error 'group-arguments "arity mismatch - missing argument: throw TypeError exception")))]))
            
;; helper functions to create ranges of numbers
;###########################
(define (cnum-range [n : number]) : (listof CExp)
  (map (lambda (x) (CNum x)) (range2 n)))

(define (vnum-range [n : number]) : (listof CVal)
  (map (lambda (x) (VNum x)) (range2 n)))

(define (range2 [n : number]) : (listof number)
  (reverse (range-backwards n)))

(define (range-backwards2 [n : number]) : (listof number)
  (cond
    [(<= n 0) empty]
    [else (cons (- n 1) (range-backwards (- n 1)))]))

(define (create-hash [keys : (listof CExp)]
                     [vals : (listof CExp)]) : (hashof CExp CExp)
  (cond
    [(and (empty? keys) (empty? vals)) (hash (list))]
    [(and (cons? keys) (cons? vals)) (hash-set (create-hash (rest keys) (rest vals)) (first keys) (first vals))]
    [else (error 'create-hash "key and value lists do not match")]))

(define (create-clist [exps : (listof CExp)]) : CExp
  (CHash (create-hash (cnum-range (length exps)) exps) (cType "list" (CId 'list))))


(define (interp-inside-class [expr : CExp] [envInstance : Env] [env : Env] [store : Store]) : AnswerC
  (type-case CExp expr
    [CFunc (args body vlist defargs classmethod vararg) (interp-func args vararg body vlist defargs (list) classmethod env store)]
    [else (interp-env expr envInstance store)]))


;; create a new class
;; THIS IS A VERY IMPORTANT FUNCTION!
(define (interp-create-class [name : symbol]
                             [body : CExp]
                             [env : Env]
                         ;    [envWithout : Env]
                             [store : Store]) : AnswerC
  (type-case AnswerC (interp-env body env store)
    [ValueA (v s) (fill-class-object name env s)]
    [BreakA (v s) (error 'interp-create-class "Break!")]
    [ContinueA (s) (error 'interp-create-class "Continue!")] ;; TODO really?
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v s) (ExceptionA (VStr "A class should not return") s)]))

;; fill-class-object gets the locals from the environment within the body of the class and
;; adds it to the 'elts' at the class object. It returns a VPass, since a class doesn't return 
;; anything, with the new store.
(define (fill-class-object [name : symbol]
                           [env : Env]
                           [store : Store]) : AnswerC
  (type-case CVal (lookupStore (lookupVar  name env) store)
    [VHash (elts uid type)
           (ValueA (VUnbound)
                   (foldl (lambda (localVar s)
                            (begin (set-box! elts (hash-set (unbox elts)
                                                            (VStr (symbol->string localVar)) 
                                                            (lookupStore (lookupEnv localVar env) s))) 
                                   s))
                          store
                          (getLocals env)))] ;; ADDED for freevar-in-method
    [else (error 'fill-class-object "when filling a class object, it should be a VHash, not anything else")]))
  

;; get instance vars
;; ADDED for freevar-in-method
(define (getInstanceVars [env : Env]) : (listof symbol)
  (filter (lambda (key)
            (type-case (optionof SLTuple) (hash-ref env key)
              [some (v) (local [(define-values (t l) v)]
                          (type-case ScopeType t
                          ;  [Instance () true]
                            [Local () true]
                            [else false]))]
              [none () (error 'fill-class-object "something terrible has happened. ")]))
          (hash-keys env)))

;;getLocals
(define (getLocals [env : Env]) : (listof symbol)
  (filter (lambda (key) 
            (type-case (optionof SLTuple) (hash-ref env key)
              [some (v) (local [(define-values (t l) v)]
                          (type-case ScopeType t
                            [Local () true]
                            [else false]))]
              [none () (error 'fill-class-object "this is just plain wrong, I always should find a value for keys from hash-keys")]))
          (hash-keys env)))
   
;;getNonLocals
(define (getNonLocals [env : Env]) : (listof symbol)
  (filter (lambda (key) 
            (type-case (optionof SLTuple) (hash-ref env key)
              [some (v) (local [(define-values (t l) v)]
                          (type-case ScopeType t
                            [NonLocal () true]
                            [else false]))]
              [none () (error 'fill-class-object "this is just plain wrong, I always should find a value for keys from hash-keys")]))
          (hash-keys env)))

;; interp application
(define (interp-VClosure-App [e : Env] 
                             [a : (listof symbol)] 
                             [varg : symbol] 
                             [b : CExp] 
                             [defargs : (listof CVal)] 
                             [args : (listof CExp)]
                             [keywargs : (listof (symbol * CExp))]
                             [star : CExp]
                             [env : Env]
                             [store : Store]) : AnswerC
  (type-case AnswerC (interp-env star env store)
    [ValueA (vh sh) (type-case CVal vh
                      [VHash (elts uid t) 
                             (let ([_grouped-args (try (group-arguments a varg args keywargs (length defargs) (hash (list)) (list))
                                                       (lambda () (list (CId '__group-arguments-exception))))])
                               (if (and (not (empty? _grouped-args)) (CId? (first _grouped-args)) (equal? (CId-x (first _grouped-args)) '__group-arguments-exception))
                                   (interp-env (CError (Make-throw 'TypeError "Something is up with the arguments..."))
                                               env
                                               sh)

                                   (interp-args-CApp b   
                                                     env
                                                     e
                                                     sh
                                                     ;a
                                                     (if (not (equal? varg 'no-vararg))
                                                         (append a (list varg))
                                                         a)
                                                     (group-arguments a varg args keywargs (length defargs) (hash (list)) (list))
                                                     (list)
                                                     defargs
                                                     ;star
                                                     )))]
                      [else (error 'interp-args-CApp "needs a hash, because star should be a list")])]
    [BreakA (v s) (error 'interp-VClosure-App "Break!")]
    [ContinueA (s) (error 'interp-VClosure-App "Continue!")]
    [ExceptionA (v s) (ExceptionA v s)]
    [ReturnA (v s) (ReturnA v s)]))

(define (transform-ctype [ctype : CType]
                         [env : Env]
                         [store : Store]) : VType
  (Type (cType-name ctype) 
        (type-case CExp (cType-basetype ctype)
          [CId (id) (lookupStore (lookupVar id env) store)]
          [CNone () (VNone)]
          [CUnbound () (VUnbound)]
          [else (error 'transform-ctype "should not be a CExp other than CId, CNone or CUnbound")])))

(define (make-index-positive [n : number] 
                             [listLen : number]) : number
  (if (< n 0)
      (+ n listLen)
      n))

(define (correct-list-subscript [val : CVal]
                                [listLen : number]) : CVal
  (type-case CVal val
    [VNum (n)
          (cond
            [(fixnum? n) (VNum (make-index-positive n listLen))]
            [else (error 'correct-list-subscript "non-Int passed as subscript for a list")])]
    [else (error 'correct-list-subscript "non-VNum passed as subscript for a list")]))

(define (interp-delete [targets : (listof CExp)]
                       [env : Env]
                       [store : Store]) : Store
  (cond
    [(empty? targets) store]
    [else
     (type-case CExp (first targets)
       [CId (id) (interp-delete (rest targets) env (augmentStore (lookupEnv id env) (VUnbound) store))]
       [CSubscript (value attr)
                   (type-case AnswerC (interp-env value env store)
                     [ValueA (v1 s1) (type-case AnswerC (interp-env attr env s1)
                                       [ValueA (v2 s2) (if (isInstanceOf v1 "_dict" env s2)
                                                           (begin (set-box! (VHash-elts v1) (hash-set (hash-remove (unbox (VHash-elts v1)) v2)
                                                                                                      (VStr "__size__")
                                                                                                      (VNum (- (VNum-n (getAttr (VStr "__size__") v1 env s2)) 1))))
                                                                  (set-box! (VHash-elts (getAttr (VStr "__keys__") v1 env s2))
                                                                            (hash-remove (unbox (VHash-elts (getAttr (VStr "__keys__") v1 env s2)))
                                                                                         v2))
                                                                  (interp-delete (rest targets) env s2))
                                                           (error 'interp-delete "we're not expecting to get anything but dictionaries and ids for delete"))]
                                       [else (error 'interp-delete "we expect that all attributes being deleted in the tests to be ValueA's")])]
                     [else (error 'interp-delete "we expect that all values being deleted in the tests to be ValueA's")])]
       [else (error 'interp-delete "we expect that delete will only receive CId's or dictionaries (CSubscript's)")])]))

(define (make-localsclass-list [arg : CVal]
                               [env : Env]
                               [store : Store]) : CVal
  (getAttr (VStr "__dict__") (lookupStore (lookupVar (string->symbol (VStr-s arg)) env) store) env store))

(define (make-localsfunc-dict [arg : CVal]
                              [env : Env]
                              [store : Store]) : CVal
  (ValueA-value (interp-env (desugar (PyDict (map (lambda (arg) (PyHolder (CHolder arg))) (fill-localsfunc-dict-keys (list) (VSymbolList-lst arg)))
                                             (map (lambda (arg) (PyHolder (CHolder arg))) (fill-localsfunc-dict-values (list) (VSymbolList-lst arg) env store))))
                                             
                            env 
                            store)))

(define (fill-localsfunc-dict-values [currentList : (listof CVal)]
                                     [localsList : (listof symbol)]
                                     [env : Env]
                                     [store : Store]) : (listof CVal)
  (cond
    [(empty? localsList) currentList]
    [else (fill-localsfunc-dict-values (append currentList (list (lookupStore (lookupEnv (first localsList) env) store)))
                                       (rest localsList)
                                       env
                                       store)]))

(define (fill-localsfunc-dict-keys [currentList : (listof CVal)]
                                   [localsList : (listof symbol)]) : (listof CVal)
  (cond
    [(empty? localsList) currentList]
    [else (fill-localsfunc-dict-keys (append currentList (list (VStr (symbol->string (first localsList)))))
                                     (rest localsList))]))

;; interp-env
(define (interp-env [expr : CExp] 
                    [env : Env] 
                    [store : Store]) : AnswerC
  (type-case CExp expr
    [CNum (n) (ValueA (VNum n) store)]
    [CStr (s) (ValueA (VStr s) store)]
    [CTrue () (ValueA (VTrue) store)]
    
    [CError (e) (type-case AnswerC (interp-env e env store)
                  [ValueA (v s) (type-case CVal v
                                  [VNone () (type-case CVal (unbox exn-to-reraise)
                                                          [VUnbound () (interp-env (CError (CApp (CId 'RuntimeError)
                                                                                                 (list (CStr "No active exception")) ;; TODO this needs to take an argument...
                                                                                                 (list)
                                                                                                 (Empty-list))) 
                                                                                   env 
                                                                                   s)]
                                                          [else (ExceptionA (unbox exn-to-reraise) s)])]
                                  [VUnbound () (interp-env (CError (CApp (CId 'RuntimeError)
                                                                         (list (CStr "No active exception")) ;; TODO this needs to take an argument...
                                                                         (list)
                                                                         (Empty-list))) 
                                                           env 
                                                           s)]
                                  [else (begin (set-box! exn-to-reraise v)
                                               (ExceptionA v s))])]
                  [BreakA (v s) (error 'CError "Should not have a break here")]
                  [ContinueA (s) (error 'CError "Should not have a continue here")]
                  [ExceptionA (v s) (ExceptionA v s)]
                  [ReturnA (v s) (error 'CError "should not get a Return statement when raising something")])]
    
    [CReturn (value) (type-case AnswerC (interp-env value env store)
                       [ValueA (v s) (ReturnA v s)]
                       [BreakA (v s) (error 'CReturn "Should not have a break here")]
                       [ContinueA (s) (error 'CReturn "Should not have a continue here")]
                       [ExceptionA (v s) (ExceptionA v s)]
                       [ReturnA (v s) (error 'interp "Return statement inside of Return...")])]
    
    [CBreak () (BreakA (VPass) store)] ;; TODO change this?
    [CContinue () (ContinueA store)]
    
    
    [CId (x) 
         (let ([_location (try (lookupVar x env)
                               (lambda () -10090))])
           (if (equal? _location -10090)
               (interp-env (CError (CApp (CId 'NameError)
                                                    (list (CStr (string-append ": " (symbol->string x))))
                                                    (list)
                                                    (CHash (hash (list)) (cType "list" (CId 'list)))))
                                      env
                                      store)
               (let ([_val (try (lookupStore (lookupVar x env) store)
                                (lambda () (VNum -10090)))])
                 (if (equal? _val (VNum -10090))
                     (interp-env (CError (CApp (CId 'UnboundLocalError)
                                                          (list (CStr (string-append ": " (symbol->string x))))
                                                          (list)
                                                          (CHash (hash (list)) (cType "list" (CId 'list)))))
                                            env
                                            store)
                     (ValueA (lookupStore (lookupVar x env) store) store)))))]
    
    [CLet (id scopeType bind body)
          (type-case AnswerC (interp-env bind env store)
            [ValueA (v s)
                    (let ([newLocation (new-loc)])
                      (interp-env body
                                  (augmentEnv id (values scopeType newLocation) env)
                                  (augmentStore newLocation 
                                                v
                                                s)))]
            [BreakA (v s) (BreakA v s)]
            [ContinueA (s) (ContinueA s)] ;; TODO really?
            [ExceptionA (v s) (ExceptionA v s)]
            [ReturnA (v s) (ReturnA v s)])] ;; This is a bit suspicious...
    
    
    [CSeq (e1 e2)
          (type-case AnswerC (interp-env e1 env store)
            [ValueA (v s)
                    (interp-env e2 env s)]
            [BreakA (v s) (BreakA v s)]
            [ContinueA (s) (ContinueA s)]
            [ExceptionA (v s) (ExceptionA v s)]
            [ReturnA (v s) (ReturnA v s)])]
    
    [CSet (id value)
          (type-case CExp id
            [CId (id-symbol) (type-case AnswerC (interp-env value env store)
                               [ValueA (v s)
                                        (ValueA v (augmentStore (lookupEnv id-symbol env)
                                                                v
                                                                s))]
                               [BreakA (v s) (error 'CSet:CId "Should not have a break here")]
                               [ContinueA (s) (error 'CSet:CId "Should not have a continue here")]
                               [ExceptionA (v s) (ExceptionA v s)]
                               [ReturnA (v s) (ReturnA v s)])]
            [CAttribute (attr objExpr) 
                        (type-case CExp objExpr
                          [CId (id-symbol) 
                               (type-case AnswerC (interp-env objExpr env store)
                                 [ValueA (v1 s1) 
                                         (type-case CVal v1
                                           [VHash (elts uid type) 
                                                  (type-case AnswerC (interp-env value env s1)
                                                    [ValueA (v2 s2) 
                                                            (ValueA v2 
                                                                    (begin (set-box! elts (hash-set (unbox elts) (VStr (symbol->string attr)) v2))
                                                                           s2))]
                                                                    
                                                    [BreakA (v s) (error 'CSet:CAttribute "Should not have a break here")]
                                                    [ContinueA (s) (error 'CSet:CAttribute "Should not have a continue here")]
                                                    [ExceptionA (v2 s2) (ExceptionA v2 s2)]
                                                    [ReturnA (v2 s2) (ReturnA v2 s2)])]
                                           [else (error 'CSet "trying to set field of non-object")])]
                                 [BreakA (v s) (error 'CSet:CAttribute "Should not have a break here")]
                                 [ContinueA (s) (error 'CSet:CAttribute "Should not have a continue here")]
                                 [ExceptionA (v1 s1) (ExceptionA v1 s1)]
                                 [ReturnA (v1 s1) (ReturnA v1 s1)])]
                          [else (error 'CSet "CAttribute has an expression in the object position")])]
            [CSubscript (objExpr attr) 
                        (type-case CExp objExpr
                          [CId (id-symbol) 
                               (type-case AnswerC (interp-env objExpr env store)
                                 [ValueA (v-obj s1) 
                                         (type-case AnswerC (interp-env attr env s1)
                                           [ValueA (v-attr s2)
                                                   (type-case AnswerC (interp-env value env s2)
                                                     [ValueA (v-value s3)
                                                             (type-case CVal v-obj
                                                               [VHash (elts uid type) 
                                                                      (cond
                                                                        [(or (isInstanceOf v-obj "list" env s3) (isInstanceOf v-obj "tuple" env s3))
                                                                         (let ([_listLen (VNum-n (getAttr (VStr "__size__") v-obj env s3))])
                                                                           (let ([_index (VNum-n (correct-list-subscript v-attr _listLen))])
                                                                             (if (< _listLen _index)
                                                                                 (interp-env (CError (CApp (CId 'IndexError)
                                                                                                           (list)
                                                                                                           (list)
                                                                                                           (Empty-list)))
                                                                                             env
                                                                                             s3)
                                                                                 (ValueA v-value 
                                                                                         (begin (set-box! elts (hash-set (unbox elts) (VNum _index) v-value))
                                                                                                (set-box! elts (hash-set (unbox elts) (VStr "__size__") (VNum (if (= _listLen _index)
                                                                                                                                                                  (+ _listLen 1)
                                                                                                                                                                  _listLen))))
                                                                                                s3)))))]
                                                                        [(isInstanceOf v-obj "_dict" env s3) ;;TODO TODO TODO handle size here...
                                                                         (try (ValueA v-value 
                                                                                      (let ([_dictlen (VNum-n (getAttr (VStr "__size__") v-obj env s3))])
                                                                                        (begin (set-box! elts (hash-set (hash-set (unbox elts) v-attr v-value)
                                                                                                                        (VStr "__size__")
                                                                                                                        (type-case (optionof CVal) (hash-ref (unbox elts) v-attr)
                                                                                                                          [some (s) (VNum _dictlen)]
                                                                                                                          [none () (VNum (+ _dictlen 1))])))
                                                                                               (set-box! (VHash-elts (getAttr (VStr "__keys__") v-obj env s3))
                                                                                                         (hash-set (unbox (VHash-elts (getAttr (VStr "__keys__") v-obj env s3))) 
                                                                                                                   v-attr
                                                                                                                   v-attr))
                                                                                               s3)))
                                                                              (lambda () (interp-env (CError (CApp (CId 'UnboundLocalError)
                                                                                                                   (list)
                                                                                                                   (list)
                                                                                                                   (CHash (hash (list)) (cType "list" (CId 'list)))))
                                                                                                     env
                                                                                                     s3)))]
                                                                        [else (interp-env (CError (CStr "tried to get a subscript from a non-list, non-dictionary")) env s3)])]
                                                               [else (error 'CSet "trying to set field of non-object")])]
                                                     [BreakA (v s) (error 'CSet:CSubscript "Should not have a break here")]
                                                     [ContinueA (s) (error 'CSet:CSubscript "Should not have a continue here")]
                                                     [ExceptionA (v3 s3) (ExceptionA v3 s3)]
                                                     [ReturnA (v3 s3) (error 'CSubscript "should not get a return statement when evaluating the assigned value")])]
                                           [BreakA (v s) (error 'CSet:CSubscript "Should not have a break here")]
                                           [ContinueA (s) (error 'CSet:CSubscript "Should not have a continue here")]
                                           [ExceptionA (v2 s2) (ExceptionA v2 s2)]
                                           [ReturnA (v2 s2) (error 'CSubscript "should not get a return statement when evaluating the attribute")])]
                                 [BreakA (v s) (error 'CSet:CSubscript "Should not have a break here")]
                                 [ContinueA (s) (error 'CSet:CSubscript "Should not have a continue here")]
                                 [ExceptionA (v1 s1) (ExceptionA v1 s1)]
                                 [ReturnA (v1 s1) (error 'CSubscript "should not get a return statement when evaluating an object")])]
                          [else (error 'CSet "CSubscript has an expression in the object position")])]
            [else (error 'interp-CSet "For now, CSet only support ids that are symbols or CAttributes or CSubscripts or Tuples")])]
    
    [CApp (func args keywargs star)
          (type-case AnswerC (interp-env func env store)
            [ValueA (vf sf)
                    (type-case CVal vf
                      [VClosure (e a varg b defargs uid classmethod) 
                                (type-case CExp func
                                  [CAttribute (attr value)
                                              (type-case AnswerC (interp-env value env sf)
                                                [ValueA (v s) (type-case CVal v
                                                                [VHash (elts uid type) 
                                                                       (if (or (equal? (Type-name type) "class") (equal? (Type-name type) "primitive-class"))
                                                                           (interp-VClosure-App e a varg b defargs (if classmethod
                                                                                                                       (append (list value) args)
                                                                                                                       args) keywargs star env sf) ;we pass sf because we don't want modifications to be passed
                                                                           (interp-VClosure-App e a varg b defargs (append (list (if classmethod
                                                                                                                                     (CHolder (Type-baseType type))
                                                                                                                                     value)) args) keywargs star env sf))]
                                                                [else (interp-env (CError (Make-throw 'TypeError "tried to get an attribute from a non-hash")) env s)])]
                                                [BreakA (v s) (error 'CApp "Should not have a break here")]
                                                [ContinueA (s) (error 'CApp "Should not have a continue here")]
                                                [ExceptionA (v s) (ExceptionA v s)]
                                                [ReturnA (v s) (error 'CAttribute "should not get an attribute from a return expression")])]
                                  ;;the new store for every function application:
                                  [else (interp-VClosure-App e a varg b defargs args keywargs star env sf)])]
                      
                      [VHash (elts uid type) 
                             (cond 
                               [(equal? (Type-name type) "class")
                                (let ([new-obj (VHash (box (hash (list (values (VStr "__class__") vf) )))
                                                      (new-uid)
                                                      (Type (type-case (optionof CVal) (hash-ref (unbox elts) (VStr "__name__"))
                                                              [none () (error 'interp-env:CApp:VHash "Class lacks __name__ field")]
                                                              [some (so) (type-case CVal so
                                                                           [VStr (s) s]
                                                                           [else (error 'interp-env:CApp:VHash "Non-string as name of class")])]) 
                                                            vf))])
                                  (begin
                                    ;;we call init and then return the new object
                                    (type-case AnswerC (interp-env (CApp (CAttribute '__init__ (CHolder new-obj)) 
                                                                         args 
                                                                         (list) 
                                                                         (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list))))
                                                                   env 
                                                                   sf)
                                      [ValueA (v-init s-init) (ValueA new-obj s-init)]
                                      [ExceptionA (v-init s-init) (ExceptionA v-init s-init)]
                                      [else (error 'interp-env:CApp:VHash:__init__ "should not receive anything that is not a ValueA or an ExceptionA")])))]
                               [(equal? (Type-name type) "primitive-class")
                                (type-case (optionof CVal) (hash-ref (unbox elts) (VStr "__convert__"))
                                  [none () (error 'interp-env:CApp:VHash "Primitive class lacks __convert__ field")]
                                  [some (so)
                                        (type-case CVal so
                                          [VClosure (e a varg b defargs uid classmethod) 
                                                    (interp-VClosure-App e a varg b defargs args keywargs star env sf)]
                                          [else (error 'interp-env:CApp:VHash "Primitive class has non-VClosure as __convert__ field")])])]
                               
                               
                               [else (type-case CVal (getAttr (VStr "__call__") vf env sf) ;; TODO mutation? This is non-mutative...
                                       [VClosure (e a varg b defargs uid classmethod) ;; CNone in line below is to remind us that we need to
                                                         ;; modify this portion of the code to allow mutation of object...
                                                         (interp-VClosure-App e a varg b defargs (cons (CNone) args) keywargs star env sf)]
                                       [else (error 'interp-env:CApp:VHash "Class has non-VClosure as __call__ field")])])]
                      
                      [else (interp-env (CError (CApp (CId 'TypeError)
                                                      (list)
                                                      (list)
                                                      (CHash (hash (list)) (cType "list" (CId 'list)))))
                                        env
                                        sf)])]
            [BreakA (v s) (error 'CApp "Should not have a break here")]
            [ContinueA (s) (error 'CApp "Should not have a continue here")]
            [ExceptionA (v s) (ExceptionA v s)]
            [ReturnA (v s) (ReturnA v s)] ;; or pass???
            )]
    
    [CHolder (hold) (ValueA hold store)]
    [CWhile (test body orelse vlist)
            (type-case AnswerC (interp-env test 
                                           env
                                           store)
              [ValueA (v s) (if (isTruthy v)
                                (type-case AnswerC (interp-env body 
                                                               env
                                                               s)
                                  [ValueA (v2 s2) (interp-env expr env s2)]
                                  [BreakA (v2 s2) (ValueA v2 s2)]
                                  [ContinueA (s2) (interp-env expr env s2)] ;; TODO maybe...
                                  [ExceptionA (v2 s2) (ExceptionA v2 s2)]
                                  [ReturnA (v2 s2) (ReturnA v2 s2)])
                                (ValueA (VPass) s))]
              [BreakA (v s) (error 'interp-CWhile "Why are we breaking from the test in interp CWhile?")]
              [ContinueA (s) (error 'interp-CWhile "Why are we continuing from the test in interp CWhile?")]
              [ExceptionA (v s) (ExceptionA v s)]
              [ReturnA (v s) (error 'interp-CWhile "Why are we returning from the test in interp CWhile?")])]
    
    [CFunc (args body vlist defargs classmethod vararg) 
           (interp-func args vararg body vlist defargs (list) classmethod env store)]
    [CDel (targets)
          (ValueA (VPass) (interp-delete targets env store))]

    [CPrim1 (prim arg) (interp-unary prim arg env store)]
    
    [CPrim2 (op e1 e2)
            (case op
              ;;boolops
              ;; These short-circuit, and so need their own system...
              ['or (interp-or e1 e2 env store)]
              ['and (interp-and e1 e2 env store)]
              ['is (interp-is e1 e2 env store)] ;; might want to think about these...
              ;  ['isNot (interp-isNot e1 e2 env store)]
              ['in (interp-in e1 e2 env store)]
              ['list+ (merge-listy-things e1 e2 env store)]
              ['tuple+ (merge-listy-things e1 e2 env store)]
              ['isinstance (interp-isinstance e1 e2 env store)]
              [else (interp-binop op e1 e2 env store)])
            ]
    
    [CIf (i t e)
         (type-case AnswerC (interp-env i env store)
           [ValueA (v s)
                   (if (isTruthy v)
                       (interp-env t env s)
                       (interp-env e env s))]
           [BreakA (v s) (error 'CIf "Should not have a break here")]
           [ContinueA (s) (error 'CIf "Should not have a continue here")]
           [ExceptionA (v s) (ExceptionA v s)]
           [ReturnA (v s) (ReturnA v s)])]
    [CNone () (ValueA (VNone) store)]
    [CFalse () (ValueA (VFalse) store)] 
    [CPass () (ValueA (VNone) store)] ;; doing nothing. We need a case for that...
    [CUnbound () (ValueA (VUnbound) store)]
    [CGlobalEnv () 
                (begin
                  (set! globalEnv (createGlobalEnv env))
                  (ValueA (VNone) store))]
    [CAttribute (attr value)
                (type-case AnswerC (interp-env value env store)
                  [ValueA (v s) (type-case CVal v
                                  [VHash (elts uid type) (ValueA (getAttr (VStr (symbol->string attr)) v env store) s)]
                                  [else (interp-env (CError (Make-throw 'TypeError "tried to get an attribute from a non-hash")) env s)])]
                  [BreakA (v s) (error 'CAttribute "Should not have a break here")]
                  [ContinueA (s) (error 'CAttribute "Should not have a continue here")]
                  [ExceptionA (v s) (ExceptionA v s)]
                  [ReturnA (v s) (error 'CAttribute "should not get an attribute from a return expression")])]
    [CSubscript (value attr)
                (type-case AnswerC (interp-env value env store)
                  [ValueA (v1 s1) 
                          (type-case AnswerC (interp-env attr env s1)
                            [ValueA (v2 s2)
                                    (type-case CVal v1
                                      [VHash (elts uid type)
                                             (cond
                                               [(or (isInstanceOf v1 "list" env s2) (isInstanceOf v1 "tuple" env s2))
                                                (let ([_listLen (VNum-n (getAttr (VStr "__size__") v1 env s2))])
                                                  (let ([_index (VNum-n (correct-list-subscript v2 _listLen))])
                                                    (if 
                                                     ;(begin (display (string-append (string-append (to-string _listLen) " e index: ") (to-string _index)))
                                                     (<= _listLen _index);)
                                                        (interp-env (CError (CApp (CId 'IndexError)
                                                                                  (list)
                                                                                  (list)
                                                                                  (CHash (hash (list)) (cType "list" (CId 'list)))))
                                                                    env
                                                                    s2)
                                                        (ValueA (getAttr (VNum _index) v1 env s2) s2))))]
                                               [(isInstanceOf v1 "_dict" env s2)
                                                (try (ValueA (getAttr v2 v1 env s2) s2)
                                                     (lambda () (interp-env (CError (CApp (CId 'UnboundLocalError)
                                                                                          (list)
                                                                                          (list)
                                                                                          (CHash (hash (list)) (cType "list" (CId 'list)))))
                                                                            env
                                                                            s2)))]
                                               [else (interp-env (CError (CStr "tried to get a subscript from a non-list, non-dictionary")) env s2)])]
                                      [else (interp-env (CError (CStr "tried to get a subscript from a non-hash CVal")) env s2)])]
                            [BreakA (v s) (error 'CSubscript "Should not have a break here")]
                            [ContinueA (s) (error 'CSubscript "Should not have a continue here")]
                            [ExceptionA (v s) (ExceptionA v s)]
                            [ReturnA (v s) (error 'CSubscript "should not get a return expression from a subscript attr")])]
                  [BreakA (v s) (error 'CSubscript "Should not have a break here")]
                  [ContinueA (s) (error 'CSubscript "Should not have a continue here")]
                  [ExceptionA (v s) (ExceptionA v s)]
                  [ReturnA (v s) (error 'CSubscript "should not get a return expression from a subscript value")])]
    
    [CHash (h type) (interp-CHash (hash-keys h) h (transform-ctype type env store) env store)]
    
    ;; Create a new class!!!!!!!!!!!!!
    [CCreateClass (name body vlist)
                  (let ([_newLoc-locals (new-loc)])
                    (let ([_newLoc-super (new-loc)])
                      (let ([_newEnv (newEnvScope (augmentEnv 'super (values (Local) _newLoc-super) (augmentEnv 'locals (values (Local) _newLoc-locals) env)) vlist (list) 'no-vararg)])
                        (type-case CVal (lookupStore (lookupVar  name env) store)
                          [VHash (elts uid type)
                                 (begin (set-box! elts (hash-set (unbox elts)
                                                                 (VStr "__dict__") ;;this just creates __dict__, but we never really work with this again
                                                                 (ValueA-value (interp-env (desugar (PyList (map (lambda (e) (PyStr (symbol->string e)))
                                                                                                                 (getLocals _newEnv)))) 
                                                                                           ;(map (lambda (e) (PyStr (symbol->string e)))
                                                                                           ;     (getLocals _newEnv))))
                                                                                           _newEnv
                                                                                           store))))
                                        (interp-create-class name 
                                                             body 
                                                             _newEnv 
                                                             (augmentStore _newLoc-super
                                                                           (VClosure _newEnv (list) 'no-vararg (CPrim1 '_super (CStr (symbol->string name))) (list) (new-uid) false)
                                                                           (augmentStore _newLoc-locals 
                                                                                         (VClosure _newEnv (list) 'no-vararg (CPrim1 '_localsClass (CStr (symbol->string name))) (list) (new-uid) false)  ;; TODO ???
                                                                                         store))))]
                          [else (error 'CCreateClass "a class should be a VHash")]))))]
                    
                  ;old ccreateclass has just this:
                  ;(interp-create-class name body (newEnvScope env vlist (list) 'no-vararg) store)]
    
    ;; exception handling
    [CTryExcept (body handlers orelse) 
                (type-case AnswerC (interp-env body env store)
                  [ValueA (v s) (interp-env orelse env s)]
                  [BreakA (v s) (BreakA v s)]
                  [ContinueA (s) (ContinueA s)]
                  [ExceptionA (v s) (if (hasMatchingException v handlers env s)
                                        (interp-handlers handlers v env s)
                                        (interp-env orelse env s))]
                  [ReturnA (v s) (ReturnA v s)])]
    [CTryFinally (body finalbody) (type-case AnswerC (interp-env body env store)
                                    [ValueA (v s) (interp-env finalbody env s)]
                                    [ExceptionA (v s) (type-case AnswerC (interp-env finalbody env s)
                                                        [ValueA (v2 s2) (ExceptionA v s2)]
                                                        [BreakA (v s) (error 'CTryFinally "Should not have a break here (???)")]
                                                        [ContinueA (s) (error 'CTryFinally "Should not have a continue here (???)")]
                                                        [ExceptionA (v2 s2) (ExceptionA v2 s2)]
                                                        [ReturnA (v2 s2) (ReturnA v2 s2)])]
                                    [BreakA (v s) (BreakA v s)]
                                    [ContinueA (s) (ContinueA s)]
                                    [ReturnA (v s) (type-case AnswerC (interp-env finalbody env s)
                                                     [ValueA (v2 s2) (ReturnA v s2)]
                                                     [BreakA (v s) (error 'CTryFinally "Should not have a break here (???)")]
                                                     [ContinueA (s) (error 'CTryFinally "Should not have a continue here (???)")]
                                                     [ExceptionA (v2 s2) (ExceptionA v2 s2)]
                                                     [ReturnA (v2 s2) (ReturnA v2 s2)])])]
    
    [else (error 'interp (string-append "Haven't implemented a case yet:\n"
                                        (to-string expr)))]
    ))

(define (bind-args args vals env)
  (cond [(and (empty? args) (empty? vals)) env]
        [(or (empty? args) (empty? vals))
         (error 'interp "Arity mismatch")]
        [(and (cons? args) (cons? vals))
         (hash-set (bind-args (rest args) (rest vals) env)
                   (first args) (first vals))]))

(define (wrap (exp : CExp)) : CExp
  (CTryExcept exp 
              (list (CExcHandler 'e (CId 'Exception) (CPrim1 'print (CApp (CId 'str)
                                                                          (list (CId 'e))
                                                                          (list)
                                                                          (Empty-list)))))
              (CPass)))

;(define (add-try-except [body : CExp] [exn : symbol] [catch : CExp]) : CExp
;  (CTryExcept body (list (CExcHandler 'e (CId exn) catch)) (CPass)))

;; regular interpret
(define (interp (expr : CExp)) : CVal
  (begin (set-box! exn-to-reraise (VUnbound)) ;; clean up exception to reraise
         (type-case AnswerC (interp-env expr (hash (list)) (hash (list)))
    [ValueA (v s) v]
    [ExceptionA (v s) (type-case AnswerC (interp-env (CApp (CId 'str)
                                                           (list (CHolder v))
                                                           (list)
                                                           (Empty-list)) 
                                                     (hash (list)) 
                                                     s)
                        [ValueA (vv ss) (error 'exception (pretty vv))] ;; really? 
                        [ExceptionA (vv ss) (error 'exception "-ception!")]
                        [else (error 'interp "Should not be getting breaks, continues, or returns here. ")])]
    [BreakA (v s) (error 'exception "A break got to the surface!")]
    [ContinueA (s) (error 'exception "A continue got to the surface!")]
    [ReturnA (v s) (VStr "Error: Return outside of function.")])))


(define env (hash (list (values 'a (values (Local) 1)) (values 'b (values (NonLocal) 2)) (values 'c (values (Global) 3)))))
(define h (hash (list (values 'x (values (Local) 1)) (values 'y (values (NonLocal) 2)))))

;(make-new-map 
;   (list (VNum 0) (VNum 1) (VNum 2) (VNum 3))
;   (list (VStr "s") (VStr "p") (VStr "a") (VStr "m")))

;(interp (CPrim1 'print (CNum 4)))