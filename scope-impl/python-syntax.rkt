#lang plai-typed

(require "python-core-syntax.rkt")
(require (typed-in racket/list [drop-right : [(listof 'a) number -> (listof 'a)]]))

(define-type PyExpr
  [PySeq (es : (listof PyExpr))]
  [PyNum (n : number)]
  [PyId (x : symbol)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr)) (keywordArgs : (listof keywargHelperType)) (star : PyExpr)]
  ;;Made by me:
  [PyStr (s : string)]
  [PyIf (test : PyExpr) (then : (listof PyExpr)) (orelse : (listof PyExpr))]
  [PyBoolop (boolop : symbol) (values : (listof PyExpr))]
  [PyUnaryOp (op : symbol) (arg : PyExpr)]
  [PyBinOp (op : symbol) (left : PyExpr) (right : PyExpr)]
  [PyCompare (left : PyExpr) (ops : (listof symbol)) (comparators : (listof PyExpr))]
  [PyPass]
  [PyNone]
  [PyRaise (exc : PyExpr)] ;(cause : PyExpr)]
  [PyGlobal (ids : (listof symbol))]
  [PyNonlocal (ids : (listof symbol))]
  [PyAttribute (attr : symbol) (value : PyExpr)]
  [PySubscript (value : PyExpr) (attr : PyExpr)]
  [PyDel (targets : (listof PyExpr))]
  [PyHolder (expr : CExp)] ;;only used to pass CExps aroudn the desugarer
  [PySlice (lower : PyExpr) (upper : PyExpr) (step : PyExpr)]
  
  ;; loops
  [PyWhile (test : PyExpr) (body : PyExpr) (orelse : PyExpr)]
  [PyFor (target : PyExpr) (iter : PyExpr) (body : PyExpr)]
  [PyListComp (elt : PyExpr) (generators : (listof PyExpr))]
  [PyComprehension (target : PyExpr) (iter : PyExpr)]
  
  [PyAssign (targets : (listof PyExpr)) (value : PyExpr)]
  [PyAugAssign (target : PyExpr) (op : symbol) (value : PyExpr)]
  
  [PySet (lhs : PyExpr) (value : PyExpr)]
  [PyModule (program : PyExpr)]
  [PyGlobalEnv]
  
  [PyLambda (arguments : PyExpr) (body : PyExpr)]
  [PyDef (name : symbol) (arguments : PyExpr) (body : PyExpr) (classmethod : boolean)] ;; deffun
  
  [PyClassDef (name : symbol) (bases : (listof PyExpr)) (body : PyExpr)]
  
  [PyArguments (args : (listof symbol)) (defaults : (listof PyExpr)) (vararg : symbol)]
  
  [PyReturn (value : PyExpr)] ;; return
  [PyBreak] ;; break
  [PyContinue] ;; continue
  
  
  ;; Lists, dict, etc
  [PyList (elts : (listof PyExpr))]
  [PyDict (keys : (listof PyExpr)) (values : (listof PyExpr))]
  [PyTuple (elts : (listof PyExpr))]
  [PyCollectionSet (elts : (listof PyExpr))]
  
  ;; Exceptions
  [PyTryExcept (body : PyExpr) (handlers : (listof PyExceptHandler)) (orelse : PyExpr)]
  [PyTryFinally (body : PyExpr) (finalbody : PyExpr)]
 ; [PyExceptHandler (name : symbol) (type : PyExpr) (body : (listof PyExpr))]
  
  [Py-NotExist] ;;THIS IS HERE ONLY SO THAT python-desugar won't complain about having completed all of the expressions
  )

#|(define-type LHS
  [LeftId (id : symbol)]
  [LeftAttribute (attr : symbol) (obj : PyExpr)]
  [LeftSubscript (obj : symbol) (subscript : PyExpr)])
|#
(define-type PyExceptHandler
  [PyExcHandler (name : symbol) (type : PyExpr) (body : PyExpr)])

(define-type keywargHelperType
  [keywarghelpertype (value1 : symbol) (value2 : PyExpr)])