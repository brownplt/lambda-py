#lang plai-typed/untyped

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

|#

(require (typed-in racket/base (number->string : (number -> string)))
         (typed-in racket/base (format : (string 'a -> string))))
(require [opaque-type-in racket/set [Set set?]])

(define-type CExpr
  [CSym (s : symbol)]
  [CTrue]
  [CFalse]
  [CNone]
  [CObject (class : CExpr) (bval : (optionof MetaVal))]
  [CGetAttr (value : CExpr) (attr : CExpr)]
  [CSetAttr (obj : CExpr) (attr : CExpr) (value : CExpr)]
  [CSeq (e1 : CExpr) (e2 : CExpr)]
  [CAssign (target : CExpr) (value : CExpr)]
  [CIf (test : CExpr) (then : CExpr) (else : CExpr)]
  [CId (x : symbol) (type : IdType)]
  [CLet (x : symbol) (type : IdType) (bind : CExpr) (body : CExpr)]
  [CApp (fun : CExpr) (args : (listof CExpr)) (stararg : (optionof CExpr))]
  [CFunc (args : (listof symbol)) (varargs : (optionof symbol)) (body : CExpr) (opt-class : (optionof symbol))] ; class name for methods
  [CWhile (test : CExpr) (body : CExpr) (orelse : CExpr)]
  [CReturn (value : CExpr)]
  [CBuiltinPrim (op : symbol) (args : (listof CExpr))]
  [CList (class : CExpr) (values : (listof CExpr))]
  [CTuple (class : CExpr) (values : (listof CExpr))]
  [CSet (class : CExpr) (values : (listof CExpr))]
  [CRaise (expr : (optionof CExpr))]
  [CTryExceptElse (try : CExpr) (exn-id : symbol) (excepts : CExpr) (orelse : CExpr)]
  [CTryFinally (try : CExpr) (finally : CExpr)]
  [CYield (expr : CExpr)]
  [CUndefined]
  [CBreak]
  [CContinue]
  [CModule (prelude : CExpr) (body : CExpr)]
  [CConstructModule (source : CExpr)])

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
  [VSym (s : symbol)]
  [VPointer (a : Address)])

(define-type MetaVal
             [MetaNone]
             [MetaNum (n : number)]
             [MetaStr (s : string)]
             [MetaList (v : (listof CVal))]
             [MetaTuple (v : (listof CVal))]
             [MetaSet (elts : Set)]
             [MetaDict (contents : (hashof CVal CVal))]
             [MetaClass (c : symbol)]
             [MetaClosure (env : Env) (args : (listof symbol)) (vararg : (optionof symbol)) (body : CExpr) (opt-class : (optionof symbol))] ; class name for methods
             [MetaCode (e : CExpr) (filename : string) (globals : (listof symbol))]
             [MetaPort (p : port)])

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
  [v*s (v : CVal) (s : Store)]
  [Return (v : CVal) (s : Store)]
  [Exception (v : CVal) (s : Store)]
  [Break (s : Store)]
  [Continue (s : Store)])

(define (alloc-result val sto)
  (local ([define l (new-loc)]
          [define new-sto (hash-set sto l val)])
   (v*s (VPointer l) new-sto)))

(define (alloc-result-list vals vpointers sto)
  (cond
    [(empty? vals) (v*s/list vpointers sto)]
    [else (type-case Result (alloc-result (first vals) sto)
            [v*s (vp s)
             (alloc-result-list (rest vals) (cons vp vpointers) s)]
            [else
             (error 'alloc-result-list "alloc-result returns non v*s Result")])]))

(define-type ResultList
  [v*s/list (vs : (listof Result)) (s : Store)]
  [Abnormal (ab : Result)])

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

(define (is-obj-ptr? val sto)
  (type-case CVal val
    [VPointer (a) (VObjectClass? (fetch-once a sto))]
    [else false]))

(define (is-fun? v)
  (and (VObjectClass? v) (some? (VObjectClass-mval v))
           (MetaClosure? (some-v (VObjectClass-mval v)))))

(define (is-fun-ptr? val sto)
  (type-case CVal val
    [VPointer (a) (is-fun? (fetch-once a sto))]
    [else false]))

(define (get-fun-mval [val : CVal] [sto : Store]) : MetaVal
  (cond
    [(is-fun-ptr? val sto) (some-v (VObjectClass-mval (fetch-ptr val sto)))]
    [else (error 'get-fun-mval (format "Not a function value: ~a\n" (list val (fetch-ptr val sto))))]))

;; fetch only once in the store
(define (fetch-once [w : Address] [sto : Store]) : CVal
  (type-case (optionof CVal) (hash-ref sto w)
             [some (v) v]
             [none () (error 'interp
                             (string-append "No value at address " (Address->string w)))]))

(define (fetch-ptr [val : CVal] [sto : Store] ) : CVal
  (type-case CVal val
    [VPointer (a) (fetch-once a sto)]
    [else (error 'interp (string-append "fetch-ptr got a non-VPointer: " (to-string val)))]))

(define (mk-exception [type : symbol] [arg : string] [env : Env] [sto : Store]) : Result
  (local [(define exn-loc (new-loc))
          (define arg-loc (new-loc))
          (define args-loc (new-loc))
          (define args-field-loc (new-loc))
          (define cls (fetch-once (some-v (lookup type env)) sto))
          (define arg-val (VObjectClass 'str (some (MetaStr arg)) (hash empty) (none)))]
    (Exception
      (VPointer exn-loc)
      (hash-set
        (hash-set
          (hash-set
           (hash-set sto arg-loc arg-val)
           args-loc (VObjectClass 'tuple (some (MetaTuple (list (VPointer arg-loc)))) (hash empty) (none)))
          args-field-loc (VPointer args-loc))
        exn-loc
        (VObjectClass 'exception (none) (hash-set (hash empty) 'args args-field-loc) (some cls))))))

(define-type ActivationRecord
  [Frame (class : (optionof CVal)) (self : (optionof CVal))])

(define-type-alias Stack (listof ActivationRecord))

;; Module is used to combine module binding name with its cooresponding object
(define-type Modules
  [Module (name : symbol) (object : CExpr)])

