#lang plai

(require "python-syntax.rkt")
(require "parse-python.rkt")
(require racket/match
         racket/list
         racket/base)

#|

Python parses as a JSON structure that we export from Python's ast
module.  You should use this file to turn it into a plai-typed data
structure that you define in python-syntax.rkt

|#

;; given a node in json form produce a symbol corresponding to its nodename
(define (nodetype->symbol nodejson)
  (string->symbol (hash-ref nodejson 'nodetype)))

(define (get-structured-python pyjson)
  (begin
  (match pyjson
    [(hash-table ('nodetype "Module") ('body expr-list))
     (PyModule (map get-structured-python expr-list))]


    [(hash-table ('nodetype "Expr") ('value expr))
     (get-structured-python expr)]

    [(hash-table ('nodetype "Call")
                 ('keywords keywords) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs starargs) ;; ignoring starargs for now
                 ('args args-list)
                 ('func func-expr))
     (if (equal? starargs #\nul)
         (PyApp (get-structured-python func-expr)
                (map get-structured-python args-list))
         (PyAppStarArg
           (get-structured-python func-expr)
           (map get-structured-python args-list)
           (get-structured-python starargs)))]

    [(hash-table ('nodetype "BinOp")
                 ('left l)
                 ('op op)
                 ('right r))
     (PyBinOp (get-structured-python l)
              (nodetype->symbol op)
              (get-structured-python r))]

    [(hash-table ('nodetype "UnaryOp")
                 ('op op)
                 ('operand operand))
     (PyUnaryOp (nodetype->symbol op)
                (get-structured-python operand))]

    [(hash-table ('nodetype "Compare")
                 ('left l)
                 ('ops ops)
                 ('comparators c))
     (PyCompOp (get-structured-python l)
               (map nodetype->symbol ops)
               (map get-structured-python c))]

    [(hash-table ('nodetype "BoolOp")
                 ('values values)
                 ('op op))
     (PyBoolOp (nodetype->symbol op) (map get-structured-python values))]

    [(hash-table ('nodetype "Name")
                 ('ctx ctx)        
                 ('id id))
     (PyId (string->symbol id) (nodetype->symbol ctx))]

    [(hash-table ('nodetype "Num")
                 ('n n))
     (PyNum n)]

    [(hash-table ('nodetype "Pass"))
     (PyPass)]

    [(hash-table ('nodetype "Str")
                 ('s str-value))
     (PyStr str-value)]

    [(hash-table ('nodetype "Lambda")
                 ('args args)
                 ('body body))
     (PyLam (map (lambda(arg)
                   (string->symbol (hash-ref arg 'arg)))
                 (hash-ref args 'args))
            (get-structured-python body))]

    [(hash-table ('nodetype "arguments")
                 ('args arg-list))
     (map get-structured-python arg-list)]

    [(hash-table ('nodetype "arg")
                 ('arg name)) 
     (string->symbol name)]

    [(hash-table ('nodetype "Assign")
                 ('value value)
                 ('targets targets))
     (PyAssign (map get-structured-python targets)
               (get-structured-python value))]

    [(hash-table ('nodetype "Raise") 
                 ('exc exc)
                 ('cause c))
     (if (char? exc)
       (PyRaise (PyPass))
       (PyRaise (get-structured-python exc)))]

    [(hash-table ('nodetype "arg")
                 ('arg arg))
     (string->symbol arg)]

    [(hash-table ('nodetype "If")
                 ('body body)
                 ('test test)
                 ('orelse orelse))
     (PyIf (get-structured-python test)
           (PySeq
             (map get-structured-python body))
           (get-structured-python orelse))]
    
    [(hash-table ('nodetype "ClassDef")
                 ('name name)
                 ('bases bases)
                 ('body body)
                 ('decorator_list dl)
                 ('keywords ks)
                 ('kwargs kwargs)
                 ('starargs sargs))
     (PyClass (string->symbol name)
              (map PyId-x (map get-structured-python bases))
              (get-structured-python body))]
    
    [(hash-table ('nodetype "FunctionDef")
                 ('name name)
                 ('body body)
                 ('decorator_list decorator-list)
                 ('args args)
                 ('returns returns))
     (cond
      ; varargs
      [(string?  (hash-ref args 'vararg))
       (PyFuncVarArg (string->symbol name)
               (map (lambda(arg) 
                      (string->symbol (hash-ref arg 'arg))) 
                    (hash-ref args 'args)) 
               (string->symbol (hash-ref args 'vararg))
             (get-structured-python body))]

      ; special case: classmethod decorator
      [(and (not (empty? decorator-list))
            (string=? "classmethod" (hash-ref (first decorator-list) 'id)))
       (PyClassFunc (string->symbol name)
               (map (lambda(arg) 
                      (string->symbol (hash-ref arg 'arg))) 
                    (hash-ref args 'args)) 
             (get-structured-python body))]

      ; regular function
      [else
       (PyFunc (string->symbol name)
               (map (lambda(arg)
                      (string->symbol (hash-ref arg 'arg)))
                    (hash-ref args 'args))
               (map (lambda(arg) 
                      (get-structured-python arg))
                    (hash-ref args 'defaults)) 
               (get-structured-python body))])]

    [(hash-table ('nodetype "Return")
                 ('value value))
     (PyReturn (get-structured-python value))]
    
    [(hash-table ('nodetype "Attribute")
                 ('value value)
                 ('attr attr)
                 ('ctx ctx))
     (PyDotField (get-structured-python value)
                 (string->symbol attr))]

    [(hash-table ('nodetype "Slice")
                 ('upper upper)
                 ('lower lower)
                 ('step step))
     (PySlice (get-structured-python lower)
              (get-structured-python upper)
              (get-structured-python step))]

    
    [(hash-table ('nodetype "Dict")
                 ('values values)
                 ('keys keys))
     (PyDict (map get-structured-python keys)
             (map get-structured-python values))]

    [(hash-table ('nodetype "While")
                 ('test test)
                 ('body body)
                 ('orelse else))
     (PyWhile (get-structured-python test)
              (get-structured-python body)
              (get-structured-python else))]

    [(hash-table ('nodetype "For")
                 ('target target)
                 ('iter iter)
                 ('body body)
                 ('orelse else))
     (PyFor (get-structured-python target)
            (get-structured-python iter)
            (get-structured-python body))]

    [(hash-table ('nodetype "ListComp")
                 ('elt elt)
                 ('generators gens))
     (PyListComp
       (get-structured-python elt)
       (map get-structured-python gens))]

    [(hash-table ('nodetype "GeneratorExp")
                 ('elt elt)
                 ('generators gens))
     (PyListComp
       (get-structured-python elt)
       (map get-structured-python gens))]

    [(hash-table ('nodetype "comprehension")
                 ('iter iter)
                 ('target target)
                 ('ifs ifs))
     (PyComprehen
       (get-structured-python target)
       (get-structured-python iter))]

    [(hash-table ('nodetype "Set")
                 ('elts elts))
     (PySet (map get-structured-python elts))]

    [(hash-table ('nodetype "List")
                 ('elts values)
                 ('ctx ctx))
     (PyList (map get-structured-python values))]

    [(hash-table ('nodetype "Tuple")
                 ('elts values)
                 ('ctx ctx))
     (PyTuple (map get-structured-python values))]

    [(hash-table ('nodetype "Subscript")
                 ('value val)
                 ('ctx context)
                 ('slice s))
     (PySubscript (get-structured-python val)
                  (nodetype->symbol context)
                  (get-structured-python s))]

    [(hash-table ('nodetype "Index")
                 ('value val))
     (get-structured-python val)]

    [(hash-table ('nodetype "TryFinally")
                 ('body body)
                 ('finalbody fbody))
     (let ([TEE (get-structured-python body)]
           [F (get-structured-python fbody)])
            (PyTryExceptElseFinally TEE empty (PyPass) F))]

    [(hash-table ('nodetype "TryExcept")
                 ('body body)
                 ('orelse else-expr)
                 ('handlers handlers))
     (let ([try (get-structured-python body)]
           [excepts (map get-structured-python handlers)]
           [orelse (get-structured-python else-expr)])
       (PyTryExceptElseFinally try excepts orelse (PyPass)))]

    [(hash-table ('nodetype "Break")) (PyBreak)]
    [(hash-table ('nodetype "ExceptHandler")
                 ('type type)
                 ('name name)
                 ('body body))
     (let ([types (get-structured-python type)])
       (let ([type-exprs 
               (cond
                  [(PyTuple? types) (PyTuple-values types)]
                  [(PyId? types) (list types)]
                  [else empty])])
         (if (string? name)
           (PyExceptAs type-exprs
                       (string->symbol name)
                       (get-structured-python body))
           (PyExcept type-exprs
                     (get-structured-python body)))))]

    [(hash-table ('nodetype "AugAssign")
                 ('op op)
                 ('target target)
                 ('value value))
     (PyAugAssign
       (nodetype->symbol op)
       (get-structured-python target)
       (get-structured-python value))]

    [(hash-table ('nodetype "Global")
                 ('names names))
     (PyGlobal
       (map string->symbol names))]

    [(hash-table ('nodetype "Nonlocal")
                 ('names names))
     (PyNonlocal
       (map string->symbol names))]

    [(hash-table ('nodetype "Delete")
                 ('targets targets))
     (PyDelete (map get-structured-python targets))]

    [(list (hash-table (k v) ...) ..2)
     (PySeq (map get-structured-python pyjson))]
    
    [(list (hash-table (k v) ...))
     (get-structured-python (first pyjson))]

    [(list) (PyPass)] 
    
    [empty (PyNone)]

    [_ (error 'parse "Haven't handled a case yet: ")])))


;; tests!
(print-only-errors true)

