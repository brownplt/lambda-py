#lang plai-typed/untyped

(require "python-syntax.rkt"
         "python-core-syntax.rkt"
         "python-lexical-syntax.rkt")
(require "util.rkt")
(require [typed-in racket (format : (string 'a -> string))])
(require [typed-in racket (flatten : ((listof (listof 'a) ) -> (listof 'b)))])
(require [typed-in racket (remove-duplicates : ((listof 'a) -> (listof 'a)))])
(require [typed-in racket
                   (call-with-exception-handler : (( 'a -> 'b) ( -> 'c) -> 'd))])
(require [typed-in racket
                   (abort-current-continuation : ('a -> 'b))])


(print-only-errors #t)

(define (default-recur) (error 'haiku "Bad error message, find and fix"))

(define haiku (call/cc (lambda (k)
           (call-with-exception-handler
            (lambda (y)
             (k y))
            (lambda ()
              (k (default-recur)
              ))))))
(define (gen-recur default special-func)
  (lambda (this-expr)
    (call/cc (lambda (k)
               (call-with-exception-handler
                (lambda (y)
                  (k
                   (begin
                     (if (equal? (to-string haiku) (to-string y))
                         (default this-expr)
                         (error 'gen-recur (to-string y))))))
                (lambda ()
                  (k (special-func this-expr))
                  ))))))

(define (pyexpr-modify-tree [expr : PyExpr] [special-func : (PyExpr -> LexExpr)]) : LexExpr
  (local (
        (define (default this-expr)
            (type-case PyExpr this-expr
                                        ; control structures
              [PyIf (test body orelse) (LexIf (recur test) (recur body) (recur orelse))]
              [PySeq (es) (LexSeq (map recur es))]
                                        ; the top level seq
              [PyModule (es) (LexModule (map recur es))]
              [PyAssign (targets value) (LexAssign (map recur targets) (recur value))]
              [PyAugAssign (op target value)
                           (LexAugAssign op (recur target) (recur value))]
                                        ; primitive literals
              [PyNum (n) (LexNum n)]
              [PyBool (b) (LexBool b)]
              [PyId (x ctx) (PyLexId x ctx)]
              [PyGlobal (ids) (PyLexGlobal ids)]
              [PyNonlocal (ids) (PyLexNonLocal ids)]
              
                                        ; exceptions and exception handling
              [PyRaise (expr) (LexRaise (recur expr))]
              [PyExcept (types  body) (LexExcept (map recur types) (recur body))]
              [PyExceptAs (types name body)
                          (LexExceptAs (map recur types) name (recur body))]
              [PyTryExceptElse (try except orelse)
                               (LexTryExceptElse (recur try)
                                                 (map recur except)
                                                 (recur orelse))]
              [PyTryFinally (try finally)
                            (LexTryFinally (recur try)
                                           (recur finally))]
              ; yield
              [PyYield (expr) (LexYield (recur expr))]
                                        ;loops 
              [PyWhile (test body orelse)
                       (LexWhile (recur test) (recur body) (recur orelse))]
              [PyFor (target iter body)
                     (LexFor (recur target) (recur iter) (recur body))]
              
                                        ; pass & assert
              [PyPass () (LexPass)]
              [PyAssert (test msg) (LexAssert (recur test) (map recur msg))]
              
                                        ; classes and objects 
              [PyClass (name bases body)
                       (LexClass (Unknown-scope) name (LexTuple (map recur bases)) (recur body))]
              [PyDotField (value attr) (LexDotField (recur value) attr)]

                                        ; operations
              [PyBinOp (left op right) (LexBinOp (recur left) op (recur right))] 
              [PyUnaryOp (op operand) (LexUnaryOp op (recur operand))]
              
              [PyCompOp (left ops comparators)
                        (LexCompOp (recur left) ops (map recur comparators))]
              [PyBoolOp (op values)
                        (LexBoolOp op (map recur values))] ;op = 'And | 'Or
              
                                        ; functions
              [PyLam (args body)
                     (LexLam args (recur body))]
              [PyFunc (name args defaults body decorators)
                      (LexFunc name args (map recur defaults) (recur body) (map recur decorators) (none))]
              [PyFuncVarArg (name args sarg body decorators)
                            (LexFuncVarArg name args sarg (recur body) (map recur decorators) (none))]
              [PyReturn (value) (LexReturn (recur value))]
              [PyApp (fun args) (LexApp (recur fun) (map recur args))]
              [PyAppStarArg (fun args stararg)
                            (LexAppStarArg (recur fun) (map recur args) (recur stararg))]
              
              [PyDelete (targets) (LexDelete (map recur targets))]
              
                                        ;
              [PySubscript (left context slice)
                           (LexSubscript (recur left) context (recur slice))]
              
              [PyListComp (body generators)
                          (LexListComp (recur body) (map recur generators))]
              [PyComprehen (target iter)
                           (LexComprehen (recur target) (recur iter))]
              
                                        ; builtin data structures
              [PyStr (s) (LexStr s)]
              [PyDict (keys values)
                      (LexDict (map recur keys) (map recur values))]
              [PyList (values)
                      (LexList (map recur values))]
              [PySlice (lower upper step)
                       (LexSlice (recur lower) (recur upper) (recur step))]
              [PyTuple (values) (LexTuple (map recur values))]
              [PyUndefined [] (LexUndefined)]
              [PySet (elts) (LexSet (map recur elts))]
              [PyImport (names asnames) (LexImport names asnames)]
              [PyImportFrom (module names asnames level)
                            (LexImportFrom module names asnames level)]
              [PyNone [] (LexNone)]
              [PyBreak [] (LexBreak)]
              [PyContinue [] (LexContinue)]))
        (define
          recur (gen-recur default special-func)
            ))
          (recur expr)
          ))



(define (lexexpr-modify-tree [expr : LexExpr] [special-func : (LexExpr -> LexExpr)]) : LexExpr
  (local (
        (define (default this-expr)
          (begin
            ;(display "we actually recur\n")
            (type-case LexExpr this-expr
                                        ; control structures
              [LexIf (test body orelse) (LexIf (recur test) (recur body) (recur orelse))]
              [LexSeq (es) (LexSeq (map recur es))]
                                        ; the top level seq
              [LexModule (es) (LexModule (map recur es))]
              [LexAssign (targets value) (LexAssign (map recur targets) (recur value))]
              [LexAugAssign (op target value)
                           (LexAugAssign op (recur target) (recur value))]
                                        ; primitive literals
              [LexNum (n) (LexNum n)]
              [LexBool (b) (LexBool b)]
              [LexBuiltinPrim (s args) (LexBuiltinPrim s (map recur args))]
              [PyLexId (x ctx) (PyLexId x ctx)]
              [PyLexGlobal (ids) (PyLexGlobal ids)]
              [PyLexNonLocal (ids) (PyLexNonLocal ids)]

              [LexInScopeLocals (ids ) (LexInScopeLocals ids)]
              [LexLocalId (x ctx) (LexLocalId x ctx)]
              [LexGlobalId (x ctx) (LexGlobalId x ctx)]
              [LexInstanceId (x ctx) (LexInstanceId x ctx)]
              [LexLocalLet (id bind body) (LexLocalLet id (recur bind) (recur body))]
              [LexGlobals (ids body)
                            (LexGlobals ids (recur body))]
              
                                        ; exceptions and exception handling
              [LexRaise (expr) (LexRaise (recur expr))]
              [LexExcept (types  body) (LexExcept (map recur types) (recur body))]
              [LexExceptAs (types name body)
                          (LexExceptAs (map recur types) name (recur body))]
              [LexTryExceptElse (try except orelse)
                                (LexTryExceptElse (recur try)
                                                  (map recur except)
                                                  (recur orelse))]
              [LexTryFinally (try finally)
                             (LexTryFinally (recur try)
                                            (recur finally))]
              ; yield
              [LexYield (expr) (LexYield (recur expr))]
                                        ;loops 
              [LexWhile (test body orelse)
                       (LexWhile (recur test) (recur body) (recur orelse))]
              [LexFor (target iter body)
                     (LexFor (recur target) (recur iter) (recur body))]
              
                                        ; pass & assert
              [LexPass () (LexPass)]
              [LexCore (e) (LexCore e)]
              [LexAssert (test msg) (LexAssert (recur test) (map recur msg))]              
              
                                        ; classes and objects 
              [LexClass (scope name bases body)
                       (LexClass scope name (recur bases) (recur body))]
              [LexDotField (value attr) (LexDotField (recur value) attr)]
              [LexExprField (value attr) (LexExprField (recur value) (recur attr))]

                                        ; operations
              [LexBinOp (left op right) (LexBinOp (recur left) op (recur right))] 
              [LexUnaryOp (op operand) (LexUnaryOp op (recur operand))]
              
              [LexCompOp (left ops comparators)
                        (LexCompOp (recur left) ops (map recur comparators))]
              [LexBoolOp (op values)
                        (LexBoolOp op (map recur values))] ;op = 'And | 'Or
              
                                        ; functions
              [LexLam (args body)
                     (LexLam args (recur body))]

              [LexFunc (name args defaults body decorators class)
                      (LexFunc name args (map recur defaults) (recur body) (map recur decorators)
                               (option-map recur class))]
              [LexFuncVarArg (name args sarg body decorators class)
                            (LexFuncVarArg name args sarg (recur body) (map recur decorators)
                                           (option-map recur class))]
              [LexReturn (value) (LexReturn (recur value))]
              [LexApp (fun args) (LexApp (recur fun) (map recur args))]
              [LexAppStarArg (fun args stararg)
                            (LexAppStarArg (recur fun) (map recur args) (recur stararg))]
              
              [LexDelete (targets) (LexDelete (map recur targets))]
              
                                        ;
              [LexSubscript (left context slice)
                           (LexSubscript (recur left) context (recur slice))]
              
              [LexListComp (body generators)
                          (LexListComp (recur body) (map recur generators))]
              [LexComprehen (target iter)
                           (LexComprehen (recur target) (recur iter))]
              
                                        ; Builtin data structures
              [LexStr (s) (LexStr s)]
              [LexDict (keys values)
                      (LexDict (map recur keys) (map recur values))]
              [LexList (values)
                      (LexList (map recur values))]
              [LexSlice (lower upper step)
                       (LexSlice (recur lower) (recur upper) (recur step))]
              [LexTuple (values) (LexTuple (map recur values))]
              [LexUndefined [] (LexUndefined)]
              [LexSet (elts) (LexSet (map recur elts))]
              [LexNone [] (LexNone)]
              [LexBreak [] (LexBreak)]
              [LexContinue [] (LexContinue)]
              [LexBlock [a b] [LexBlock a (recur b)]]
              [LexImport [names asnames] (LexImport names asnames)]
              [LexImportFrom [module names asnames level]
                             (LexImportFrom module names asnames level)])))
        (define recur
            (gen-recur default special-func)))
          (let ((ret (recur expr)))
            (begin
              ;(display "done\n")
              ret
          ))))



(define (pyexpr-fold-tree [expr : PyExpr] [special-func : (PyExpr -> (listof 'a))]) : (listof 'a)
  (local (
        (define (default this-expr)
            (type-case PyExpr this-expr
                                        ; control structures
              [PyIf (test body orelse)
                    (flatten (list (recur test) (recur body) (recur orelse)))]
              [PySeq (es) (flatten (map recur es))]
                                        ; the top level seq
              [PyModule (es) (flatten (map recur es))]
              [PyAssign (targets value)
                        (flatten (list (map recur targets) (list (recur value))))]
              [PyAugAssign (op target value)
                           (flatten (list (recur target) (recur value)))]
                                        ; primitive literals
              [PyNum (n) empty]
              [PyBool (b) empty]
              [PyId (x ctx)  empty]
              [PyGlobal (ids) empty]
              [PyNonlocal (ids) empty]
              
                                        ; exceptions and exception handling
              [PyRaise (expr) (flatten (recur expr))]
              [PyExcept (types  body)
                        (flatten (list (map recur types) (list (recur body))))]
              [PyExceptAs (types name body)
                          (flatten (list (map recur types) (list (recur body))))]
              [PyTryExceptElse (try except orelse)
                               (flatten (list
                                          (list (recur try))
                                          (map recur except)
                                          (list (recur orelse))))]
              [PyTryFinally (try finally)
                            (flatten (list
                                       (list (recur try))
                                       (list (recur finally))))]
              ; yield
              [PyYield (expr) (flatten (recur expr))]

                                        ;loops 
              [PyWhile (test body orelse)
                       (flatten (list (recur test) (recur body) (recur orelse)))]
              [PyFor (target iter body)
                     (flatten (list (recur target) (recur iter) (recur body)))]
              
                                        ; pass & assert
              [PyPass () empty]
              [PyAssert (test msg)
                        (flatten (list (list (recur test)) (map recur msg)))]
              
                                        ; classes and objects 
              [PyClass (name bases body)
                       (flatten (list (map recur bases) (recur body)))]
              [PyDotField (value attr)  (recur value)]

                                        ; operations
              [PyBinOp (left op right) (flatten (list (recur left) (recur right)))] 
              [PyUnaryOp (op operand) (recur operand)]
              
              [PyCompOp (left ops comparators)
                        (flatten (list (list (recur left)) (map recur comparators)))]
              [PyBoolOp (op values)
                        (flatten (map recur values))] ;op = 'And | 'Or
              
                                        ; functions
              [PyLam (args body)
                     (recur body)]
              [PyFunc (name args defaults body decorators)
                      (flatten (list (map recur defaults) (list (recur body))))]
              [PyFuncVarArg (name args sarg body decorators)
                            (recur body)]
              [PyReturn (value) (recur value)]
              [PyApp (fun args) (flatten (list (list (recur fun)) (map recur args)))]
              [PyAppStarArg (fun args stararg)
                            (flatten (list (list (recur fun))
                                           (map recur args)
                                           (list (recur stararg))))]
              
              [PyDelete (targets) (flatten (map recur targets))]
              
                                        ;
              [PySubscript (left context slice)
                           (flatten (list (recur left) (recur slice)))]
              
              [PyListComp (body generators)
                          (flatten (list (list (recur body)) (map recur generators)))]
              [PyComprehen (target iter)
                           (flatten (list (recur target) (recur iter)))]
              
                                        ; builtin data structures
              [PyStr (s)  empty]
              [PyDict (keys values)
                      (flatten (list (map recur keys) (map recur values)))]
              [PyList (values)
                      (flatten (map recur values))]
              [PySlice (lower upper step)
                       (flatten (list (recur lower) (recur upper) (recur step)))]
              [PyTuple (values) (flatten (map recur values))]
              [PyUndefined [] empty]
              [PySet (elts) (flatten (map recur elts))]
              [PyNone [] empty]
              [PyBreak [] empty]
              [PyContinue [] empty]
              [PyImport (names asnames) empty]
              [PyImportFrom (module names asnames level) empty]))
        (define (recur expr)
            ((gen-recur default special-func) expr)))
          (recur expr)
          ))


(define (lexexpr-fold-tree [expr : LexExpr] [special-func : (LexExpr -> (listof 'a))]) : (listof 'a)
  (local (
        (define (default [this-expr : LexExpr])
            (type-case LexExpr this-expr
                                        ; control structures
              [LexIf (test body orelse)
                    (flatten (list (recur test) (recur body) (recur orelse)))]
              [LexSeq (es) (flatten (map recur es))]
                                        ; the top level seq
              [LexModule (es) (flatten (map recur es))]
              [LexAssign (targets value)
                        (flatten (list (map recur targets) (list (recur value))))]
              [LexAugAssign (op target value)
                           (flatten (list (recur target) (recur value)))]
                                        ; primitive literals
              [LexNum (n) empty]
              [LexBool (b) empty]
              [LexBuiltinPrim (s args) (map recur args)]
              [LexInstanceId (x ctx)  empty]
              [LexGlobalId (x ctx)  empty]
              [LexLocalId (x ctx)  empty]
              [LexLocalLet (id bind body) (flatten (list (recur bind) (recur body)))]
              [LexInScopeLocals (_) empty]
              [LexGlobals (ids body) (recur body)]
              [PyLexId (x ctx)  empty]
              [PyLexGlobal (ids) empty]
              [PyLexNonLocal (ids) empty]
              
                                        ; exceptions and exception handling
              [LexRaise (expr) (flatten (recur expr))]
              [LexExcept (types  body)
                        (flatten (list (map recur types) (list (recur body))))]
              [LexExceptAs (types name body)
                          (flatten (list (map recur types) (list (recur body))))]
              [LexTryExceptElse (try except orelse)
                                (flatten (list
                                           (list (recur try))
                                           (map recur except)
                                           (list (recur orelse))))]
              [LexTryFinally (try finally)
                             (flatten (list
                                        (list (recur try))
                                        (list (recur finally))))]
              ; yield
              [LexYield (expr) (flatten (recur expr))]
                                        ;loops 
              [LexWhile (test body orelse)
                       (flatten (list (recur test) (recur body) (recur orelse)))]
              [LexFor (target iter body)
                     (flatten (list (recur target) (recur iter) (recur body)))]
              
                                        ; pass & assert
              [LexPass () empty]
              [LexCore (_) empty]
              [LexAssert (test msg)
                    (flatten (list (list (recur test)) (map recur msg)))]
              
                                        ; classes and objects 
              [LexClass (scope name bases body)
                       (flatten (list (recur bases) (recur body)))]
              [LexDotField (value attr)  (recur value)]
              [LexExprField (value attr)  (flatten (list (recur value) (recur attr)))]

                                        ; operations
              [LexBinOp (left op right) (flatten (list (recur left) (recur right)))] 
              [LexUnaryOp (op operand) (recur operand)]
              
              [LexCompOp (left ops comparators)
                        (flatten (list (list (recur left)) (map recur comparators)))]
              [LexBoolOp (op values)
                        (flatten (map recur values))] ;op = 'And | 'Or
              
                                        ; functions
              [LexLam (args body)
                     (recur body)]
              [LexFunc (name args defaults body decorators class)
                      (flatten (list (map recur defaults) (list (recur body) (type-case (optionof LexExpr) class
                                                          [some (v) (recur v)]
                                                          [none () empty]))))]
              [LexFuncVarArg (name args sarg body decorators class)
                            (flatten (list (recur body) (type-case (optionof LexExpr) class
                                                          [some (v) (recur v)]
                                                          [none () empty])))]
              [LexReturn (value) (recur value)]
              [LexApp (fun args) (flatten (list (list (recur fun)) (map recur args)))]
              [LexAppStarArg (fun args stararg)
                            (flatten (list (list (recur fun))
                                           (map recur args)
                                           (list (recur stararg))))]
              
              [LexDelete (targets) (flatten (map recur targets))]
              
                                        ;
              [LexSubscript (left context slice)
                           (flatten (list (recur left) (recur slice)))]
              
              [LexListComp (body generators)
                          (flatten (list (list (recur body)) (map recur generators)))]
              [LexComprehen (target iter)
                           (flatten (list (recur target) (recur iter)))]
              
                                        ; builtin data structures
              [LexStr (s)  empty]
              [LexDict (keys values)
                      (flatten (list (map recur keys) (map recur values)))]
              [LexList (values)
                      (flatten (map recur values))]
              [LexSlice (lower upper step)
                       (flatten (list (recur lower) (recur upper) (recur step)))]
              [LexTuple (values) (flatten (map recur values))]
              [LexUndefined [] empty]
              [LexSet (elts) (flatten (map recur elts))]
              [LexNone [] empty]
              [LexBreak [] empty]
              [LexContinue [] empty]
              [LexBlock [a b] (recur b)]
              [LexImport (names asnames) empty]
              [LexImportFrom (module names asnames level) empty]))
        (define (recur expr) ((gen-recur default special-func) expr)
            ))
          (recur expr)
          ))

