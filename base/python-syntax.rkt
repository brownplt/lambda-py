#lang plai-typed

(define-type PyExpr
  ; control structures
  [PyIf (test : PyExpr) (body : PyExpr) (orelse : PyExpr)]
  [PySeq (es : (listof PyExpr))]
  ; the top level seq
  [PyModule (es : (listof PyExpr))]
  [PyAssign (targets : (listof PyExpr)) (value : PyExpr)]
  [PyAugAssign (op : symbol) (target : PyExpr) (value : PyExpr)]

  ; primitive literals
  [PyNum (n : number)]
  [PyBool (b : boolean)]
  [PyId (x : symbol) (ctx : symbol)]
  [PyGlobal (ids : (listof symbol))]
  [PyNonlocal (ids : (listof symbol))]

  ; exceptions and exception handling
  [PyRaise (expr : PyExpr)]
  [PyExcept (types : (listof PyExpr)) (body : PyExpr)]
  [PyExceptAs (types : (listof PyExpr)) (name : symbol) (body : PyExpr)]
  [PyTryExceptElseFinally (try : PyExpr) (except : (listof PyExpr))
                          (orelse : PyExpr) (finally : PyExpr)]

  ;loops 
  [PyWhile (test : PyExpr) (body : PyExpr) (orelse : PyExpr)]
  [PyFor (target : PyExpr) (iter : PyExpr) (body : PyExpr)]
  
  ; pass
  [PyPass]

  ; classes and objects 
  [PyClass (name : symbol) (bases : (listof symbol)) (body : PyExpr)]
  [PyDotField (value : PyExpr) (attr : symbol)]

  ; operations
  [PyBinOp (left : PyExpr) (op : symbol) (right : PyExpr)] ;op = 'Add | 'Sub | etc
  [PyUnaryOp (op : symbol) (operand : PyExpr)]

  [PyCompOp (left : PyExpr) 
            (ops : (listof symbol)) ;ops = 'Eq | 'NotEq | 'Lt etc
            (comparators : (listof PyExpr))]
  [PyBoolOp (op : symbol) (values : (listof PyExpr))] ;op = 'And | 'Or

  ; functions
  [PyLam (args : (listof symbol)) (body : PyExpr)]
  [PyFunc (name : symbol) (args : (listof symbol)) (defaults : (listof PyExpr)) (body : PyExpr)]
  [PyClassFunc (name : symbol) (args : (listof symbol)) (body : PyExpr)]
  [PyFuncVarArg (name : symbol) (args : (listof symbol)) 
                (sarg : symbol) (body : PyExpr)]
  [PyReturn (value : PyExpr)]
  [PyApp (fun : PyExpr) (args : (listof PyExpr))]
  [PyAppStarArg (fun : PyExpr) (args : (listof PyExpr)) (stararg : PyExpr)]

  [PyDelete (targets : (listof PyExpr))]

  ;
  [PySubscript (left : PyExpr) (context : symbol) (slice : PyExpr)]

  [PyListComp (body : PyExpr) (generators : (listof PyExpr))]
  [PyComprehen (target : PyExpr) (iter : PyExpr)]

  ; builtin data structures
  [PyStr (s : string)]
  [PyDict (keys : (listof PyExpr)) (values : (listof PyExpr))]
  [PyList (values : (listof PyExpr))]
  [PySlice (lower : PyExpr) (upper : PyExpr) (step : PyExpr)]
  [PyTuple (values : (listof PyExpr))]
  [PyUndefined]
  [PySet (elts : (listof PyExpr))]
  [PyNone]
  [PyBreak]

)

