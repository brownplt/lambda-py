#lang plai-typed/untyped

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
  [PyTryExceptElse (try : PyExpr) (except : (listof PyExpr)) (orelse : PyExpr)]
  [PyTryFinally (try : PyExpr) (finally : PyExpr)]

  ; yield
  [PyYield (expr : PyExpr)]

  ;loops 
  [PyWhile (test : PyExpr) (body : PyExpr) (orelse : PyExpr)]
  [PyFor (target : PyExpr) (iter : PyExpr) (body : PyExpr)]
  
  ; pass & assert
  [PyPass]
  [PyAssert (test : PyExpr) (msg : (listof PyExpr))]

  ; classes and objects 
  [PyClass (name : symbol) (bases : (listof PyExpr)) (body : PyExpr)]
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
  [PyFunc (name : symbol) (args : (listof symbol)) (defaults : (listof PyExpr)) 
          (body : PyExpr) (decorators : (listof PyExpr))]
  [PyFuncVarArg (name : symbol) (args : (listof symbol)) 
                (sarg : symbol) (body : PyExpr) (decorators : (listof PyExpr))]
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
  [PyContinue]

  ; import, which desugar to asname = __import__("name")
  [PyImport (names : (listof string)) (asnames : (listof symbol))]
  ; from import, which desugar to 
  ; 1. _temp = __import__(...)
  ; 2. asname = _temp.name
  [PyImportFrom (module : string) 
                (names : (listof string))
                (asnames : (listof symbol))
                (level : number)]
)


