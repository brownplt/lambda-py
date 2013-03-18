#lang plai-typed/untyped

(define-type LocalOrGlobal
  [Locally-scoped]
  [Globally-scoped]
  [Instance-scoped]
  [Unknown-scope])

(define-type LexExpr
  ; control structures
  [LexIf (test : LexExpr) (body : LexExpr) (orelse : LexExpr)]
  [LexSeq (es : (listof LexExpr))]
  ; the top level seq
  [LexModule (es : (listof LexExpr))]
  [LexAssign (targets : (listof LexExpr)) (value : LexExpr)]
  [LexAugAssign (op : symbol) (target : LexExpr) (value : LexExpr)]

  ; primitive literals
  [LexNum (n : number)]
  [LexBool (b : boolean)]

  ;for embedded primitives in lexical.
  [LexBuiltinPrim (s : symbol) (args : (listof LexExpr))]

  ; exceptions and exception handling
  [LexRaise (expr : LexExpr)]
  [LexExcept (types : (listof LexExpr)) (body : LexExpr)]
  [LexExceptAs (types : (listof LexExpr)) (name : symbol) (body : LexExpr)]
  [LexTryExceptElse (try : LexExpr) (except : (listof LexExpr))
                          (orelse : LexExpr)]
  [LexTryFinally (try : LexExpr) (finally : LexExpr)]

  ;loops 
  [LexWhile (test : LexExpr) (body : LexExpr) (orelse : LexExpr)]
  [LexFor (target : LexExpr) (iter : LexExpr) (body : LexExpr)]
  
  ; pass & assert
  [LexPass]
  [LexAssert (test : LexExpr) (msg : (listof LexExpr))]

  ; classes and objects 
  [LexClass (scope : LocalOrGlobal) (name : symbol) (bases : LexExpr) (body : LexExpr)]
  [LexDotField (value : LexExpr) (attr : symbol)]
  [LexExprField (value : LexExpr) (attr : LexExpr)]

  ; operations
  [LexBinOp (left : LexExpr) (op : symbol) (right : LexExpr)] ;op = 'Add | 'Sub | etc
  [LexUnaryOp (op : symbol) (operand : LexExpr)]

  [LexCompOp (left : LexExpr) 
            (ops : (listof symbol)) ;ops = 'Eq | 'NotEq | 'Lt etc
            (comparators : (listof LexExpr))]
  [LexBoolOp (op : symbol) (values : (listof LexExpr))] ;op = 'And | 'Or

  ; functions
  [LexLam (args : (listof symbol)) (body : LexExpr)]
  [LexFunc (name : symbol) (args : (listof symbol)) (defaults : (listof LexExpr))
           (body : LexExpr) (decorators : (listof LexExpr)) (class : (optionof LexExpr))]
  [LexFuncVarArg (name : symbol) (args : (listof symbol)) 
                (sarg : symbol) (body : LexExpr) (decorators : (listof LexExpr)) (class : (optionof LexExpr))]
  [LexReturn (value : LexExpr)]
  [LexApp (fun : LexExpr) (args : (listof LexExpr))]
  [LexAppStarArg (fun : LexExpr) (args : (listof LexExpr)) (stararg : LexExpr)]

  [LexDelete (targets : (listof LexExpr))]

  ;
  [LexSubscript (left : LexExpr) (context : symbol) (slice : LexExpr)]

  [LexListComp (body : LexExpr) (generators : (listof LexExpr))]
  [LexComprehen (target : LexExpr) (iter : LexExpr)]

  ;new identifiers for scope.
  [LexLocalId (x : symbol) (ctx : symbol)]
  [LexGlobalId (x : symbol) (ctx : symbol)]
  [LexInstanceId (x : symbol) (ctx : symbol)]
  [LexGlobals (ids : (listof symbol)) (body : LexExpr)]
  [LexLocalLet (id : symbol) (bind : LexExpr) (body : LexExpr)]
  [LexInScopeLocals (locals : (listof symbol))]
  

  ;helpful; a "block" denotes a new scope
  ;the nonlocals list is for function arguments.
  [LexBlock (nonlocals : (listof symbol)) (body : LexExpr) ]


  ;old identifiers left in for compatability during migration
  ;still have the "Py" naming convention to stress that they're
  ;not really in this language.
  [PyLexId (x : symbol) (ctx : symbol)]
  [PyLexGlobal (globals : (listof symbol))]
  [PyLexNonLocal (locals : (listof symbol))]

  
  ; builtin data structures
  [LexStr (s : string)]
  [LexDict (keys : (listof LexExpr)) (values : (listof LexExpr))]
  [LexList (values : (listof LexExpr))]
  [LexSlice (lower : LexExpr) (upper : LexExpr) (step : LexExpr)]
  [LexTuple (values : (listof LexExpr))]
  [LexUndefined]
  [LexSet (elts : (listof LexExpr))]
  [LexNone]
  [LexBreak]
  [LexContinue]

  ; import, which desugar to asname = __import__("name")
  [LexImport (names : (listof string)) (asnames : (listof symbol))]
  ; from import, which desugar to 
  ; 1. _temp = __import__(...)
  ; 2. asname = _temp.name
  [LexImportFrom (module : string) 
                 (names : (listof string))
                 (asnames : (listof symbol))
                 (level : number)]

)

