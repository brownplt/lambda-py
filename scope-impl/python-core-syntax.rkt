#lang plai-typed

#|

This is the core language; it is just borrowing a few things from 
ParselTongue.

|#

(define-type CExp
  [CNum (n : number)]
  [CStr (s : string)]
  [CTrue]
  [CSeq (e1 : CExp) (e2 : CExp)]
  [CError (e1 : CExp)]
  [CIf (test : CExp) (then : CExp) (else : CExp)]
  [CId (x : symbol)]
  [CLet (id : symbol) (scopeType : ScopeType) (bind : CExp) (body : CExp)]
  [CApp (fun : CExp) (args : (listof CExp)) (keywordArguments : (listof (symbol * CExp))) (star : CExp)]
  [CFunc (args : (listof symbol)) 
         (body : CExp) 
         (vlist : (listof (ScopeType * symbol))) 
         (defargs : (listof CExp))
         (classmethod : boolean)
         (vararg : symbol)]
  [CPrim1 (prim : symbol) (arg : CExp)]
  [CPrim2 (op : symbol) (e1 : CExp) (e2 : CExp)]
  [CFalse]
  [CNone]
  [CPass]
  [CReturn (value : CExp)]
  [CBreak]
  [CContinue]
  [CSet (id : CExp) (value : CExp)]
  [CAttribute (attr : symbol) (value : CExp)]
  [CSubscript (value : CExp) (attr : CExp)]
  [CDel (targets : (listof CExp))]
  ;; loops
  [CWhile (test : CExp) (body : CExp) (orelse : CExp) (vlist : (listof (ScopeType * symbol)))]
  
  ;[CBind (bind : (ScopeType * symbol))] ;;puts an identifier in the environment but does nothing in the store.
  [CUnbound]
  [CGlobalEnv]
  
  [CHash (elts : (hashof CExp CExp)) (type : CType)]
  
  ;; type to help create a new class
  [CCreateClass (name : symbol) (body : CExp) (vlist : (listof (ScopeType * symbol)))]
  
  ;; Exception handling types
  [CTryExcept (body : CExp) (handlers : (listof CExceptionHandler)) (orelse : CExp)]
  [CTryFinally (body : CExp) (finalbody : CExp)]
  
  [CHolder (hold : CVal)]
  
  
  [C-NotExist (a : number)] ;;THIS IS HERE ONLY SO THAT python-interp won't complain about having completed all of the expressions
  )

(define (Empty-list) : CExp
  (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list)))) ;; convenience function

(define (Make-throw [ex : symbol] [msg : string]) : CExp
  (CApp (CId ex)
        (list (CStr msg))
        (list)
        (Empty-list)))

(define-type CExceptionHandler
  [CExcHandler (name : symbol) (type : CExp) (body : CExp)])

(define-type CVal
  [VSymbolList (lst : (listof symbol))]
  [VNum (n : number)]
  [VStr (s : string)]
  [VTrue]
  [VClosure (env : Env) (args : (listof symbol)) (vararg : symbol) (body : CExp) (defargs : (listof CVal)) (uid : Uid) (classmethod : boolean)]
  
  [VNone]
  [VFalse]
  [VPass]
  [VUnbound]
  
  [VHash (elts : (boxof (hashof CVal CVal))) (uid : Uid) (type : VType)]
  
  )

(define-type VType
  [Type (name : string) (baseType : CVal)])

(define-type CType
  [cType (name : string) (basetype : CExp)]) ;;this is used to create a VType from the desugarer

(define-type-alias Location number)
(define-type ScopeType
 ; [Instance] ;; ADDED for freevar-in-method
  [Local]
  ;[NotReallyLocal]
  [NonLocal]
  [Global])

(define-type-alias Uid number)

(define-type-alias SLTuple (ScopeType * number))
(define-type-alias Env (hashof symbol SLTuple))
(define-type-alias Store (hashof Location CVal))

(define-type AnswerC
  [ValueA (value : CVal) (store : Store)]
  [ExceptionA (value : CVal) (store : Store)]
  [ReturnA (value : CVal) (store : Store)]
  [BreakA (value : CVal) (store : Store)]
  [ContinueA (store : Store)]
  )
 