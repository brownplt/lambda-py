#lang plai-typed/untyped

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

|#

(require (typed-in racket/base (number->string : (number -> string))))
(require [opaque-type-in racket/set [Set set?]])

(define-type CExpr
  [CStr (s : string)]
  [CTrue]
  [CFalse]
  [CNone]
  [CClass (name : CExpr) (bases : (listof symbol)) (body : CExpr)]
  [CObject (class : symbol) (bval : (optionof MetaVal))]
  [CGetField (value : CExpr) (attr : symbol)]
  [CSeq (e1 : CExpr) (e2 : CExpr)]
  [CAssign (target : CExpr) (value : CExpr)]
  [CIf (test : CExpr) (then : CExpr) (else : CExpr)]
  [CId (x : symbol) (type : IdType)]
  [CLet (x : symbol) (type : IdType) (bind : CExpr) (body : CExpr)]
  [CApp (fun : CExpr) (args : (listof CExpr)) (stararg : (optionof CExpr))]
  [CFunc (args : (listof symbol)) (varargs : (optionof symbol)) (body : CExpr)
         (opt-class : (optionof symbol))] ; class name for methods
  [CWhile (test : CExpr) (body : CExpr) (orelse : CExpr)]
  [CReturn (value : CExpr)]
  [CPrim1 (prim : symbol) (arg : CExpr)]
  [CPrim2 (prim : symbol) (arg1 : CExpr) (arg2 : CExpr)]
  [CBuiltinPrim (op : symbol) (args : (listof CExpr))]
  [CList (class : CExpr) (values : (listof CExpr))]
  [CTuple (class : CExpr) (values : (listof CExpr))]
  [CDict (class : CExpr) (contents : (hashof CExpr CExpr))]
  [CSet (class : CExpr) (values : (listof CExpr))]
  [CRaise (expr : (optionof CExpr))]
  [CTryExceptElseFinally (try : CExpr) (excepts : (listof CExpr))
                         (orelse : CExpr) (finally : CExpr)]
  [CExcept (types : (listof CExpr)) (name : (optionof symbol)) (body : CExpr)]
  [CUndefined]
  [CBreak]
  [CContinue]
  [CModule (prelude : CExpr) (body : CExpr)])

(define-type IdType
    [GlobalId]
    [LocalId])

(define-type IdPair
    [idpair (id : symbol) (type : IdType)])

(define-type-alias IdEnv (listof IdPair))

(define-type-alias port 'port)

(define-type CVal
  [VObjectClass (antecedent : symbol) (mval : (optionof MetaVal))
                (dict : object-dict) (class : (optionof CVal))]
  [VUndefined]
  [VPointer (a : Address)]
  [VClosure (env : Env) (args : (listof symbol)) (vararg : (optionof symbol)) (body : CExpr)
            (opt-class : (optionof symbol))]) ; class name for methods

(define-type MetaVal
             [MetaNum (n : number)]
             [MetaStr (s : string)]
             [MetaList (v : (listof CVal))]
             [MetaTuple (v : (listof CVal))]
             [MetaDict (contents : (hashof CVal CVal))]
             [MetaClass (c : symbol)]
             [MetaSet (elts : Set)]
             [MetaNone]
             [MetaPort (p : port)])

;; env is a listof hashof's so there are deliniations between closures
(define-type-alias Env (listof (hashof symbol Address)))

(define-type-alias Address number)
(define Address->string number->string)
(define-type-alias Store (hashof Address CVal))
(define-values (new-loc reset-loc)
  (let ([n (box 0)])
    (values
      (lambda ()
        (begin
          (set-box! n (add1 (unbox n)))
          (unbox n)))
      (lambda ()
        (set-box! n 0)))))

(define-type Result
  [v*s (v : CVal) (s : Store) (a : (optionof Address))]
  [Return (v : CVal) (s : Store) (a : (optionof Address))]
  [Exception (v : CVal) (s : Store)]
  [Break (s : Store)]
  [Continue (s : Store)])

(define-type-alias object-dict (hashof symbol Address))

(define (lookup [x : symbol] [env : Env]) : (optionof Address)
  (cond
    [(empty? env) (none)]
    [else (type-case (optionof Address) (hash-ref (first env) x)
            [some (v) (some v)]
            [none () (lookup x (rest env))])]))

;; lookup in just the local environment
(define (lookup-local [x : symbol] [env : Env]) : (optionof Address)
  (hash-ref (first env) x))

(define (lookup-global [x : symbol] [env : Env]) : (optionof Address)
  (cond
    [(empty? (rest env)) (hash-ref (first env) x)]
    [else (lookup-global x (rest env))]))

(define (fetch [w : Address] [sto : Store]) : CVal
  (local [(define val 
            (type-case (optionof CVal) (hash-ref sto w)
              [some (v) v]
              [none () (error 'interp
                              (begin ;(display sto)
                              (string-append "No value at address " (Address->string w))))]))]
    (if (VPointer? val)
        (fetch (VPointer-a val) sto)
        val)))

;; fetch only once in the store
(define (fetch-once [w : Address] [sto : Store]) : CVal
  (type-case (optionof CVal) (hash-ref sto w)
             [some (v) v]
             [none () (error 'interp
                             (string-append "No value at address " (Address->string w)))]))

;; lookup in the env and sto, in order to get the final address through the aliasing address.
(define (deep-lookup [x : symbol] [env : Env] [sto : Store]) : (optionof Address)
  (let ((env-addr (lookup x env)))
    (if (some? env-addr)
        (let ((mayb-pointer (fetch-once (some-v env-addr) sto)))
          (if (VPointer? mayb-pointer)
              (some (VPointer-a mayb-pointer))
              env-addr))               
        (none))))

(define (deep-lookup-global [x : symbol] [env : Env] [sto : Store]) : (optionof Address)
  (let ((env-addr (lookup-global x env)))
    (if (some? env-addr)
        (let ((mayb-pointer (fetch-once (some-v env-addr) sto)))
          (if (VPointer? mayb-pointer)
              (some (VPointer-a mayb-pointer))
              env-addr))
        (none))))

(define (deep-lookup-local [x : symbol] [env : Env] [sto : Store]) : (optionof Address)
  (let ((env-addr (lookup-local x env)))
    (if (some? env-addr)
        (let ((mayb-pointer (fetch-once (some-v env-addr) sto)))
          (if (VPointer? mayb-pointer)
              (some (VPointer-a mayb-pointer))
              env-addr))
        (none))))

(define-type ActivationRecord
  [Frame (env : Env) (class : (optionof CVal)) (self : (optionof CVal))])

(define-type-alias Stack (listof ActivationRecord))
