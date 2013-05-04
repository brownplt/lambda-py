#lang plai

(require "python-syntax.rkt")
(require racket/match
         racket/list
         racket/base)
(require (only-in plai-typed some none))

#|

Python parses as a JSON structure that we export from Python's ast
module.  You should use this file to turn it into a plai-typed data
structure that you define in python-syntax.rkt

|#

;; given a node in json form produce a symbol corresponding to its nodename
(define (nodetype->symbol nodejson)
  (string->symbol (ast-ref nodejson 'nodetype)))

(define (parse-func name body decorator-list args)
  (PyFunc (string->symbol name)
          (map (lambda(arg)
                 (string->symbol (ast-ref arg 'arg)))
               (ast-ref args 'args))
          (if (string?  (ast-ref args 'vararg))
              (some (string->symbol (ast-ref args 'vararg)))
              (none))
          (map (lambda(arg)
                 (string->symbol (ast-ref arg 'arg)))
               (ast-ref args 'kwonlyargs))
          (if (string?  (ast-ref args 'kwarg))
              (some (string->symbol (ast-ref args 'kwarg)))
              (none))
          (map get-structured-python
               (remove #\nul (ast-ref args 'defaults)))
          (map get-structured-python
               (remove #\nul (ast-ref args 'kw_defaults)))
          (get-structured-python body)
          (map get-structured-python decorator-list)))

#| 
Source locations - see python-parser.rkt

Source location (line/column/position/span) is being built gradually in the parser by wrapping real AST nodes in "SrcLoc" AST nodes. The "SrcLoc" AST is put in a parameter while walking the tree. Use ast-ref instead of hash-ref to skip "SrcLoc" nodes and get concrete properties.
|#

(define current-src-pos (make-parameter #f))

;; Get concrete (non-srcloc) property of AST ast identified by key (symbol) by skipping SrcLoc nodes.
(define (ast-ref ast key)
  (cond
   [(equal? "SrcLoc" (hash-ref ast 'nodetype))
    (ast-ref (hash-ref ast 'ast) key)]
   [else (hash-ref ast key)]))

(define (get-structured-python pyjson)
  (begin
  (match pyjson
    [(hash-table ('nodetype "SrcLoc") 
                 ('ast inner-pyjson)
                 (_ _) ...)
     (parameterize [(current-src-pos pyjson)]
       (get-structured-python inner-pyjson))]

    [(hash-table ('nodetype "Module") ('body expr-list))
     (PyModule (map get-structured-python expr-list))]

    [(hash-table ('nodetype "Expr") ('value expr))
     (get-structured-python expr)]

    [(hash-table ('nodetype "Call")
                 ('keywords keywords)
                 ('kwargs kwargs)
                 ('starargs starargs)
                 ('args args-list)
                 ('func func-expr))
     (PyApp (get-structured-python func-expr)
            (map get-structured-python args-list)
            (map get-structured-python keywords)
            (if (equal? starargs #\nul)
                (none)
                (some (get-structured-python starargs)))
            (if (equal? kwargs #\nul)
                (none)
                (some (get-structured-python kwargs))))]

    [(hash-table ('nodetype "keyword")
                 ('arg name)
                 ('value val))
     (PyTuple (list (PyStr name)
                    (get-structured-python val)))]

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
                   (string->symbol (ast-ref arg 'arg)))
                 (ast-ref args 'args))
            (if (string?  (ast-ref args 'vararg))
                (some (string->symbol (ast-ref args 'vararg)))
                (none))
            (map (lambda(arg)
                   (string->symbol (ast-ref arg 'arg)))
                 (ast-ref args 'kwonlyargs))
            (if (string?  (ast-ref args 'kwarg))
                (some (string->symbol (ast-ref args 'kwarg)))
                (none))
            (map get-structured-python
                 (remove #\nul (ast-ref args 'defaults)))
            (map get-structured-python
                 (remove #\nul (ast-ref args 'kw_defaults)))
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

    [(hash-table ('nodetype "Assert")
                 ('test test)
                 ('msg msg))
     (PyAssert (get-structured-python test)
               (if (char? msg) (list) (list (get-structured-python msg))))]

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

    [(hash-table ('nodetype "IfExp")
                 ('body body)
                 ('test test)
                 ('orelse orelse))
     (PyIf (get-structured-python test)
           (get-structured-python body)
           (get-structured-python orelse))]
    
    [(hash-table ('nodetype "ClassDef")
                 ('name name)
                 ('bases bases)
                 ('body body)
                 ('decorator_list decorator-list)
                 ('keywords keywords)
                 ('kwargs kwargs)
                 ('starargs starargs))
     (PyClass (string->symbol name)
              (map get-structured-python bases)
              (get-structured-python body)
              (map get-structured-python keywords)
              (if (equal? starargs #\nul)
                  (none)
                  (some (get-structured-python starargs)))
              (if (equal? kwargs #\nul)
                  (none)
                  (some (get-structured-python kwargs)))
              (map get-structured-python decorator-list))]
    
    [(hash-table ('nodetype "FunctionDef")
                 ('name name)
                 ('body body)
                 ('decorator_list decorator-list)
                 ('args args)
                 ('returns returns))
     (parse-func name body decorator-list args)]

    [(hash-table ('nodetype "FunctionDef")
                 ('name name)
                 ('body body)
                 ('decorator_list decorator-list)
                 ('args args))
     (parse-func name body decorator-list args)]
    
    [(hash-table ('nodetype "Yield")
                 ('value val))
     (PyYield (get-structured-python val))]
    
    [(hash-table ('nodetype "Return")
                 ('value value))
     (PyReturn (if (equal? #\nul value)
                   (none)
                   (some (get-structured-python value))))]
    
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
            (get-structured-python body)
            (get-structured-python else))]

    [(hash-table ('nodetype "ListComp")
                 ('elt elt)
                 ('generators gens))
     (PyListComp
       (get-structured-python elt)
       (map get-structured-python gens))]

    [(hash-table ('nodetype "SetComp")
                 ('elt elt)
                 ('generators gens))
     (PyApp (PyId '%set 'Load)
            (list (PyListComp
                   (get-structured-python elt)
                   (map get-structured-python gens)))
            (list) (none) (none))]

    [(hash-table ('nodetype "DictComp")
                 ('key key)
                 ('value value)
                 ('generators gens))
     (PyApp (PyId '%dict 'Load)
            (list (PyListComp
                   (PyTuple (list (get-structured-python key)
                                  (get-structured-python value)))
                   (map get-structured-python gens)))
            (list) (none) (none))]

    [(hash-table ('nodetype "GeneratorExp")
                 ('elt elt)
                 ('generators gens))
     (PyGeneratorExp
       (get-structured-python elt)
       (map get-structured-python gens))]

    [(hash-table ('nodetype "comprehension")
                 ('iter iter)
                 ('target target)
                 ('ifs ifs))
     (PyComprehen
       (get-structured-python target)
       (get-structured-python iter)
       (map get-structured-python ifs))]

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
            (PyTryFinally TEE F))]

    [(hash-table ('nodetype "TryExcept")
                 ('body body)
                 ('orelse else-expr)
                 ('handlers handlers))
     (let ([try (get-structured-python body)]
           [excepts (map get-structured-python handlers)]
           [orelse (get-structured-python else-expr)])
       (PyTryExceptElse try excepts orelse))]

    [(hash-table ('nodetype "With")
                 ('body body)
                 ('context_expr context)
                 ('optional_vars target))
     (PyWith (get-structured-python context)
             (if (equal? target #\nul)
                (none)
                (some (get-structured-python target)))
             (get-structured-python body))]

    [(hash-table ('nodetype "Break")) (PyBreak)]

    [(hash-table ('nodetype "Continue")) (PyContinue)]

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

    [(hash-table ('nodetype "Import")
                 ('names names))
                                        ; when asname is empty, `name`
                                        ; will be used as `asname`
     (PyImport (map (lambda (x)
                      (ast-ref x 'name)) ; name is string type
                    names)
               (map (lambda (x)
                      (let ([as (ast-ref x 'asname)])
                        (if (equal? as #\nul)
                            (string->symbol (ast-ref x 'name))
                            (string->symbol as))))
                    names))]
    
    ; largely the same with PyImport.
    [(hash-table ('nodetype "ImportFrom")
                 ('module module)
                 ('names names)
                 ('level level))
     (PyImportFrom module
                   (map (lambda (x)
                          (ast-ref x 'name))
                        names)
                   (map (lambda (x)
                          (let ([as (ast-ref x 'asname)])
                            (if (equal? as #\nul)
                                (string->symbol (ast-ref x 'name))
                                (string->symbol as))))
                        names)
                   level)]

    [(list (hash-table (k v) ...) ..2)
     (PySeq (map get-structured-python pyjson))]
    
    [(list (hash-table (k v) ...))
     (get-structured-python (first pyjson))]

    [(list) (PyPass)] 

    [#\nul (PyNone)]
    
    [_ (error 'parse (string-append 
                      (let-values (((src-line src-column src-position)
                                    (if (current-src-pos)
                                        (values (hash-ref (current-src-pos) 'line)
                                                (hash-ref (current-src-pos) 'column)
                                                (hash-ref (current-src-pos) 'position))
                                        (values #f #f #f))))
                        (format "Haven't handled a case yet (Line ~a, Column ~a, Position ~a):~n"
                                src-line src-column src-position))
				    (pretty-format pyjson)
				    ))])))

;; tests!
(print-only-errors true)


