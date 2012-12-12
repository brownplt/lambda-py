#lang plai-typed

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
  [CClass (name : symbol) (base : symbol) (body : CExpr)]
  [CObject (class : symbol) (bval : (optionof MetaVal))]
  [CGetField (value : CExpr) (attr : symbol)]
  [CSeq (e1 : CExpr) (e2 : CExpr)]
  [CAssign (target : CExpr) (value : CExpr)]
  [CError (e1 : CExpr)]
  [CIf (test : CExpr) (then : CExpr) (else : CExpr)]
  [CId (x : symbol) (type : IdType)]
  [CLet (x : symbol) (bind : CExpr) (body : CExpr)]
  [CApp (fun : CExpr) (args : (listof CExpr)) (stararg : (optionof CExpr))]
  [CFunc (args : (listof symbol)) (varargs : (optionof symbol)) (body : CExpr)
         (method : boolean)]
  [CWhile (test : CExpr) (body : CExpr) (orelse : CExpr)]
  [CReturn (value : CExpr)]
  [CPrim1 (prim : symbol) (arg : CExpr)]
  [CPrim2 (prim : symbol) (arg1 : CExpr) (arg2 : CExpr)]
  [CBuiltinPrim (op : symbol) (args : (listof CExpr))]
  [CList (values : (listof CExpr))]
  [CTuple (values : (listof CExpr))]
  [CDict (contents : (hashof CExpr CExpr))]
  [CSet (values : (listof CExpr))]
  [CRaise (expr : (optionof CExpr))]
  [CTryExceptElseFinally (try : CExpr) (excepts : (listof CExpr))
                         (orelse : CExpr) (finally : CExpr)]
  [CExcept (types : (listof CExpr)) (name : (optionof symbol)) (body : CExpr)]
  [CUndefined]
  [CBreak]
  [CModule (prelude : CExpr) (body : CExpr)])

(define-type IdType
    [GlobalId]
    [NonlocalId]
    [LocalId])

(define-type IdPair
    [idpair (id : symbol) (type : IdType)])

(define-type-alias IdEnv (listof IdPair))

(define-type CVal
  [VObject (antecedent : symbol) (mval : (optionof MetaVal)) (dict : object-dict)]
  [VClosure (env : Env) (args : (listof symbol)) (vararg : (optionof symbol)) (body : CExpr)]
  [VUndefined])

(define-type MetaVal
             [MetaNum (n : number)]
             [MetaStr (s : string)]
             [MetaList (v : (listof CVal))]
             [MetaTuple (v : (listof CVal))]
             [MetaDict (contents : (hashof CVal CVal))]
             [MetaClass (c : symbol)]
             [MetaSet (elts : Set)]
             [MetaNone])

;; env is a listof hashof's so there are deliniations between closures
(define-type-alias Env (listof (hashof symbol Address)))

(define-type-alias Address number)
(define Address->string number->string)
(define-type-alias Store (hashof Address CVal))
(define new-loc
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (unbox n)))))

(define-type Result
  [v*s*e (v : CVal) (s : Store) (e : Env)]
  [Return (v : CVal) (s : Store) (e : Env)]
  [Exception (v : CVal) (s : Store) (e : Env)]
  [Break (s : Store) (e : Env)])

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

;; lookup for nonlocal variables:
;;   skip the current scope level
;;   don't go to the global scope level
(define (lookup-nonlocal [x : symbol] [env : Env]) : (optionof Address)
  (local [(define rec-lookup-nonlocal
            (λ ([x : symbol] [env : Env]) : (optionof Address)
               (cond
                 [(empty? (rest env)) (none)]
                 [else (type-case (optionof Address) (hash-ref (first env) x)
                         [some (v) (some v)]
                         [none () (lookup x (rest env))])])))]
    (rec-lookup-nonlocal x (rest env))))


(define (fetch [w : Address] [sto : Store]) : CVal
  (type-case (optionof CVal) (hash-ref sto w)
    [some (v) v]
    [none () (error 'interp (string-append "No value at address " (Address->string w)))]))

