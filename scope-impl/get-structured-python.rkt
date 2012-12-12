#lang plai

(require "python-syntax.rkt")
(require racket/match
         racket/list)

#|

Python parses as a JSON structure that we export from Python's ast
module.  You should use this file to turn it into a plai-typed data
structure that you define in python-syntax.rkt

|#

(define (get-structured-python pyjson)
  (match pyjson
    [(hash-table ('nodetype "Module") ('body expr-list))
     (PyModule (PySeq (map get-structured-python expr-list)))]
    [(hash-table ('nodetype "Expr") ('value expr))
     (get-structured-python expr)]
    
    
    [(hash-table ('nodetype "Call")
                 ('keywords #\nul) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs #\nul) ;; ignoring starargs for now
                 ('args args-list)
                 ('func func-expr))
     (PyApp (get-structured-python func-expr)
            (map get-structured-python args-list)
            (list)
            (PyList (hash (list)))
            )]
    
    ;;The base case for "Call" outputs an empty list for keyword arguments
    [(hash-table ('nodetype "Call")
                 ('keywords #\nul) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs starargs) ;; ignoring starargs for now
                 ('args args-list)
                 ('func func-expr))
     (PyApp (get-structured-python func-expr)
            (map get-structured-python args-list)
            (list)
            (get-structured-python starargs)
            )]
    
    [(hash-table ('nodetype "Call")
                 ('keywords keywords) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs #\nul) ;; ignoring starargs for now
                 ('args args-list)
                 ('func func-expr))
     (PyApp (get-structured-python func-expr)
            (map get-structured-python args-list)
            (map get-structured-python keywords)
            (PyList (list))
            )]
    
    [(hash-table ('nodetype "Call")
                 ('keywords keywords) ;; ignoring keywords for now
                 ('kwargs kwargs)     ;; ignoring kwargs for now
                 ('starargs starargs) ;; ignoring starargs for now
                 ('args args-list)
                 ('func func-expr))
     (PyApp (get-structured-python func-expr)
            (map get-structured-python args-list)
            (map get-structured-python keywords)
            (get-structured-python starargs)
            )]
    
    [(hash-table ('nodetype "keyword")
                 ('arg arg)
                 ('value value))
     (keywarghelpertype (string->symbol arg) (get-structured-python value))]
    
    ;; Catching None up here, before we hit the identifer case
    [(hash-table ('nodetype "Name")
                 ('ctx _)
                 ('id "None"))
     (PyNone)]
    
    ;; Should these cases be in lib? This is something to ask at design check...   
    ;; Alternatively, they could be handled in desugaring. This is a decision we need to make
    ;; before we can proceed. I vote for desugaring. 
    
    ;; Stuff for classes
    [(hash-table ('nodetype "ClassDef")
                 ('name name)
                 ('bases bases)
                 ('keywords keywords)
                 ('starargs starargs)
                 ('kwargs kwargs)
                 ('body body)
                 ('decorator_list decorator_list))
     (PyClassDef (string->symbol name)
                 (map get-structured-python bases)
                 (PySeq (map get-structured-python body)))]
    
    [(hash-table ('nodetype "Name")
                 ('ctx _)        ;; ignoring ctx for now
                 ('id id))
     (PyId (string->symbol id))]
    [(hash-table ('nodetype "Num")
                 ('n n))
     (PyNum n)]
    ;;MADE BY ME:
    [(hash-table ('nodetype "If")
                 ('body body) ;stmt-list
                 ('test test) ;expr
                 ('orelse orelse)) ;stmt-list
     (PyIf (get-structured-python test )
           (map get-structured-python body)
           (map get-structured-python orelse))]
    [(hash-table ('nodetype "BoolOp")
                 ('op op) ;boolop
                 ('values values)) ;expr-list
     (PyBoolop (get-structured-python op) (map get-structured-python values))]
    [(hash-table ('nodetype "UnaryOp")
                 ('op op)
                 ('operand operand))
     (PyUnaryOp (get-structured-python op) (get-structured-python operand))]
    [(hash-table ('nodetype "Compare")
                 ('left left) ;expr
                 ('ops ops) ;cmpop*
                 ('comparators comparators)) ;expr*
     (PyCompare (get-structured-python left)
                (map get-structured-python ops)
                (map get-structured-python comparators))]
    [(hash-table ('nodetype "Str")
                 ('s s))
     (PyStr s)]
    [(hash-table ('nodetype "Pass"))
     (PyPass)]
    
    
    
    [(hash-table ('nodetype "Lambda")
                 ('args args) ;arguments
                 ('body body)) ;expr
     (PyLambda (get-structured-python args)
               (get-structured-python body))]
    [(hash-table ('nodetype "arguments")
                 ('args args) ;arg*
                 ('vararg vararg)
                 ('varargannotation varargannotation)
                 ('kwonlyargs kwonlyargs)
                 ('kwarg kwarg)
                 ('kwargannotation kwargannotation)
                 ('defaults defaults)
                 ('kw_defaults kw_defaults))
     (PyArguments (map get-structured-python args)
                  (map get-structured-python defaults)
                  (if (string? vararg)
                      (string->symbol vararg)
                      'no-vararg))]
    [(hash-table ('nodetype "arg")
                 ('arg arg)
                 ('annotation annotation))
     (string->symbol arg)]
    [(hash-table ('nodetype "Assign")
                 ('targets targets)
                 ('value value))
     (PyAssign (map get-structured-python targets)
               (get-structured-python value))]
    
    [(hash-table ('nodetype "AugAssign")
                 ('target target)
                 ('op op)
                 ('value value))
     (PyAugAssign (get-structured-python target)
                  (get-structured-python op)
                  (get-structured-python value))]
    ;; raise
    [(hash-table ('nodetype "Raise")    ;;check if this is wrong
                 ('exc #\nul)
                 ('cause cause))
     (PyRaise (PyNone))] ;; (get-structured-python cause))]
    [(hash-table ('nodetype "Raise")
                 ('exc exc)
                 ('cause cause))
     (PyRaise (get-structured-python exc))] ;; (get-structured-python cause))]
    [(hash-table ('nodetype "BinOp")
                 ('left left) ;expr
                 ('op op) ;operator
                 ('right right)) ;expr
     (PyBinOp (get-structured-python op)
              (get-structured-python left)
              (get-structured-python right))]
    
    [(hash-table ('nodetype "Delete")
                 ('targets targets))
     (PyDel (map get-structured-python targets))]
    
    [(hash-table ('nodetype "Set")
                 ('elts elts))
     (PyCollectionSet (map get-structured-python elts))]
    
    ;; def
    [(hash-table ('nodetype "FunctionDef")
                 ('name name)
                 ('args args)
                 ('body body)
                 ('decorator_list decorator-list)
                 ('returns returns))
     (PyDef (string->symbol name) 
            (get-structured-python args)
            (PySeq (map get-structured-python body))
            (if (equal? decorator-list #\nul)
                false
                true))]
    
    ;; return case
    [(hash-table ('nodetype "Return")
                 ('value value))
     (PyReturn (get-structured-python value))]
    
    ;; break and continue
    [(hash-table ('nodetype "Break"))
     (PyBreak)]
    
    [(hash-table ('nodetype "Continue"))
     (PyContinue)]
    
    
    ;; global variable
    [(hash-table ('nodetype "Global")
                 ('names names))
     (PyGlobal (map (lambda (name) (string->symbol name)) names))]
    
    ;; nonlocal variable
    [(hash-table ('nodetype "Nonlocal")
                 ('names names))
     (PyNonlocal (map (lambda (name) (string->symbol name)) names))]
    
    ;; Attribute
    [(hash-table ('nodetype "Attribute")
                 ('value value)
                 ('attr attr)
                 ('ctx ctx))
     (PyAttribute (string->symbol attr)
                  (get-structured-python value))]
    
    ;; Subscript
    [(hash-table ('nodetype "Subscript")
                 ('value value)
                 ('slice slice)
                 ('ctx ctx))
     (PySubscript (get-structured-python value)
                  (get-structured-python slice))]
    
    [(hash-table ('nodetype "Slice")
                 ('lower lower)
                 ('upper upper)
                 ('step step))
     (PySlice (if (equal? lower #\nul)
                  (PyNone)
                  (get-structured-python lower))
              (if (equal? upper #\nul)
                  (PyNone)
                  (get-structured-python upper))
              (if (equal? step #\nul)
                  (PyNum 1)
                  (get-structured-python step)))]
    
    ;; Index (this is only used in subscripts
    [(hash-table ('nodetype "Index")
                 ('value value))
     (get-structured-python value)]
    
    ;; lists
    [(hash-table ('nodetype "List")
                 ('elts elts)
                 ('ctx ctx))
     (PyList (map get-structured-python elts))]
    
    ;; Dicts
    [(hash-table ('nodetype "Dict")
                 ('keys keys)
                 ('values values))
     (PyDict (map get-structured-python keys)
             (map get-structured-python values))]
    
    ;; Tuples
    [(hash-table ('nodetype "Tuple")
                 ('elts elts)
                 ('ctx ctx))
     (PyTuple (map get-structured-python elts))]
    
    ;; TryExcept - WITHOUT ELSE
    [(hash-table ('nodetype "TryExcept")
                 ('body body)
                 ('handlers handlers)
                 ('orelse '()))
     (PyTryExcept (PySeq (map get-structured-python body))
                  (map get-structured-python handlers)
                  (PyNone))]
    ;; TryExcept - WITH ELSE
    [(hash-table ('nodetype "TryExcept")
                 ('body body)
                 ('handlers handlers)
                 ('orelse orelse))
     (PyTryExcept (PySeq (map get-structured-python body))
                  (map get-structured-python handlers)
                  (PySeq (map get-structured-python orelse)))]
    
    ;; TryFinally
    [(hash-table ('nodetype "TryFinally")
                 ('body body)
                 ('finalbody finalbody))
     (PyTryFinally (PySeq (map get-structured-python body))
                   (PySeq (map get-structured-python finalbody)))]
    
    ;; ExceptHandlers
    [(hash-table ('nodetype "ExceptHandler")          ;;CHECK IF THIS IS CORRECT
                 ('name #\nul)
                 ('type #\nul)
                 ('body body))
     (PyExcHandler 'no-name
                   (PyNone)
                   (PySeq (map get-structured-python body)))]
    ;; ExceptHandlers
    [(hash-table ('nodetype "ExceptHandler")
                 ('name name)
                 ('type type)
                 ('body body))
     (PyExcHandler (if (string? name)
                       (string->symbol name)
                       'no-name)
                   (get-structured-python type)
                   (PySeq (map get-structured-python body)))]
    
    ;; Attribute --------------------------------------------------Create a PyAttribute--------------------------------------------
    [(hash-table ('nodetype "Attribute")
                 ('value value)
                 ('attr attr)
                 ('ctx ctx))
     (PyPass)]
    
    
    ;; Loops
    [(hash-table ('nodetype "While")
                 ('body body)
                 ('orelse orelse)
                 ('test test))
     (PyWhile (get-structured-python test)
              (PySeq (map get-structured-python body))
              (PySeq (cons (PyPass) (map get-structured-python orelse))))]
    
    [(hash-table ('nodetype "For")
                 ('target target)
                 ('iter iter)
                 ('body body)
                 ('orelse orelse))
     (PyFor (get-structured-python target)
            (get-structured-python iter)
            (PySeq (map get-structured-python body)))]
    
    [(hash-table ('nodetype "ListComp")
                 ('elt elt)
                 ('generators generators))
     (PyListComp (get-structured-python elt)
                 (map get-structured-python generators))]
    
    [(hash-table ('nodetype "GeneratorExp")
                 ('elt elt)
                 ('generators generators))
     (PyListComp (get-structured-python elt)
                 (map get-structured-python generators))]
    
    [(hash-table ('nodetype "comprehension")
                 ('target target)
                 ('iter iter)
                 ('ifs ifs))
     (PyComprehension (get-structured-python target)
                      (get-structured-python iter))]
                 
    ;;THE ONES THAT RETURN PRIMITIVES (symbols, numbers, strings, etc):
    
    ;; arithmetic
    [(hash-table ('nodetype "Add"))
     'python-add]
    [(hash-table ('nodetype "Sub"))
     'python-sub]
    [(hash-table ('nodetype "Mult"))
     'python-mult]
    [(hash-table ('nodetype "Div"))
     'python-div]
    [(hash-table ('nodetype "FloorDiv"))
     'python-floor-div]
    [(hash-table ('nodetype "Mod"))
     'python-mod]
 ;   [(hash-table ('nodetype "Pow")) ;; this will need to be recursive. Wait to implement...
 ;    'python-pow]
    
    [(hash-table ('nodetype "Or"))
     'or]
    [(hash-table ('nodetype "And"))
     'and]
    [(hash-table ('nodetype "Eq"))
     'python-eq]
    [(hash-table ('nodetype "NotEq"))
     'python-notEq]
    [(hash-table ('nodetype "Lt"))
     'python-lt]
    [(hash-table ('nodetype "LtE"))
     'python-lte]
    [(hash-table ('nodetype "Gt"))
     'python-gt]
    [(hash-table ('nodetype "GtE"))
     'python-gte]
    [(hash-table ('nodetype "Is"))
     'python-is]
    [(hash-table ('nodetype "IsNot"))
     'python-isNot]
    [(hash-table ('nodetype "In"))
     'python-in]
    [(hash-table ('nodetype "NotIn"))
     'python-notIn]
    [(hash-table ('nodetype "BitAnd"))
     'python-bitand]
    [(hash-table ('nodetype "BitOr"))
     'python-bitor]
    [(hash-table ('nodetype "BitXor"))
     'python-bitxor]

    
    ;; Unary
    [(hash-table ('nodetype "Not"))
     'python-not]
    [(hash-table ('nodetype "USub"))
     'python-negate]
    [(hash-table ('nodetype "UAdd"))
     'python-uadd]
    [(hash-table ('nodetype "Invert"))
     'python-invert]
    
    
    
                 
    [_ (begin (display "PARSING: Haven't handled a case yet: \n")
              (pretty-write pyjson)
              (error 'parse "error is described above."))]))

