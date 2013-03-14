#lang plai-typed/untyped

(require "python-core-syntax.rkt"
         "builtins/num.rkt"
         "builtins/str.rkt"
         "builtins/list.rkt"
         "builtins/tuple.rkt"
         "builtins/dict.rkt"
         "builtins/object.rkt"
         "builtins/bool.rkt"
         "builtins/set.rkt"
         "builtins/none.rkt"
         "builtins/file.rkt"
         "builtins/code.rkt"
         "builtins/module.rkt"
         "util.rkt"
         "modules/builtin-modules.rkt"
         (typed-in "get-structured-python.rkt"
                   (get-structured-python : ('a -> 'b)))
         (typed-in "parse-python.rkt"
                   (parse-python/port : ('a string -> 'b)))
         (typed-in racket/base (open-input-file : ('a -> 'b)))
         "python-syntax.rkt"
         "python-lexical-syntax.rkt"
         "python-desugar.rkt"
         (typed-in racket/base (append : ((listof 'a) (listof 'a) (listof 'a) (listof 'a) (listof 'a) -> (listof 'a)))))


(define-type LibBinding
  [bind (left : symbol) (right : CExpr)])

(define base-exception
  (seq-ops
    (list 
      (CAssign (CId 'BaseException (GlobalId))
               (builtin-class
                 'BaseException
                 (list 'object)
                 (CNone)))
      (def 'BaseException '__init__
           (CFunc (list 'self) (some 'args)
                  (CAssign 
                    (CGetField
                      (CId 'self (LocalId))
                      'args)
                    (CId 'args (LocalId)))
                  (some 'BaseException)))
      (def 'BaseException '__str__
           (CFunc (list 'self) (none)
                  (CReturn
                    (CBuiltinPrim 'exception-str
                                  (list (CId 'self (LocalId)))))
                  (some 'BaseException))))))

(define len-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self (LocalId))
          '__len__)
        (list)
        (none)))
    (none)))

(define min-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self (LocalId))
          '__min__)
        (list)
        (none)))
    (none)))

(define max-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self (LocalId))
          '__max__)
        (list)
        (none)))
    (none)))

(define abs-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self (LocalId))
          '__abs__)
        (list)
        (none)))
    (none)))

(define callable-lambda
  (local [(define exn-id (new-id))]
    (CFunc (list 'to-check) (none)
           (CSeq
             (CTryExceptElse
               ;; try to get __call__ attribute and return True
               (CSeq
                 (CGetField (CId 'to-check (LocalId)) '__call__)
                 (CReturn (CTrue)))
               exn-id 
               (default-except-handler exn-id (CNone))
               (CNone))
             ;; use the primary operator
             (CReturn (CPrim1 'callable (CId 'to-check (LocalId)))))
           (none))))

(define locals-lambda
  (CFunc (list) (none)
         (CReturn
          (CBuiltinPrim '$locals empty))
         (none)))

;; TODO: if source contains null bytes, it should raise TypeError
(define compile-lambda
  (CFunc (list 'source 'filename 'mode) (none)
         (CReturn
          (CBuiltinPrim 'compile
                        (list
                         (CId 'source (LocalId))
                         (CId 'filename (LocalId))
                         (CId 'mode (LocalId)))))
         (none)))

(define make_module-lambda
  (CFunc (list 'code) (none)
         (CReturn
          (CConstructModule (CId 'code (LocalId))))
         (none)))

(define lib-functions
  (list (bind 'True (assign 'True (CTrue)))
        (bind 'False (assign 'False (CFalse)))
        (bind 'None (assign 'None (CNone)))

        (bind 'object object-class)
        (bind '%object (assign '%object (CId 'object (GlobalId))))
        (bind 'num num-class)
        (bind '%num (assign '%num (CId 'num (GlobalId))))
        (bind 'int int-class)
        (bind '%int (assign '%int (CId 'int (GlobalId))))
        (bind 'float float-class)
        (bind '%float (assign '%float (CId 'float (GlobalId))))
        (bind 'code code-class)
        (bind '%code (assign '%code (CId 'code (GlobalId))))
        (bind '$module module-class)

        (bind 'compile (assign 'compile compile-lambda))
        (bind '%compile (assign '%compile (CId 'compile (GlobalId))))
        (bind 'make_module (assign 'make_module make_module-lambda))
        (bind '%make_module (assign '%make_module (CId 'make_module (GlobalId))))
        
        (bind 'len (assign 'len len-lambda))
        (bind 'min (assign 'min min-lambda))
        (bind 'max (assign 'max max-lambda))
        (bind 'abs (assign 'abs abs-lambda))

        (bind 'callable (assign 'callable callable-lambda))
        (bind '%callable (assign '%callable (CId 'callable (GlobalId))))
        (bind 'locals (assign 'locals locals-lambda))

        (bind 'BaseException base-exception)
        (bind 'Exception (assign 'Exception (make-exception-class 'Exception)))
        (bind 'ImportError (assign 'ImportError (make-exception-class 'ImportError)))
        (bind 'IOError (assign 'IOError (make-exception-class 'IOError)))
        (bind 'NameError (assign 'NameError (make-exception-class 'NameError)))
        (bind 'TypeError (assign 'TypeError (make-exception-class 'TypeError)))
        (bind 'ValueError (assign 'ValueError (make-exception-class 'ValueError)))
        (bind 'SyntaxError (assign 'SyntaxError (make-exception-class 'SyntaxError)))
        (bind 'AttributeError (assign 'AttributeError (make-exception-class 'AttributeError)))
        (bind 'RuntimeError (assign 'RuntimeError (make-exception-class 'RuntimeError)))
        (bind '$Reraise (assign '$Reraise (make-exception-class '$Reraise)))
        (bind 'KeyError (assign 'KeyError (make-exception-class 'KeyError)))
        (bind 'IndexError (assign 'IndexError (make-exception-class 'IndexError)))
        (bind 'UnboundLocalError
              (assign 'UnboundLocalError (make-exception-class 'UnboundLocalError)))
        (bind 'ZeroDivisionError
              (assign 'ZeroDivisionError (make-exception-class 'ZeroDivisionError)))
        (bind 'StopIteration (assign 'StopIteration (make-exception-class 'StopIteration)))
        (bind 'AssertionError (assign 'AssertionError (make-exception-class 'AssertionError)))))

(define lib-function-dummies
  (append
      (map (lambda(b) (bind (bind-left b) (CUndefined)))
           lib-functions)
      (list 
            (bind 'none (CUndefined))
            (bind '%none (CUndefined))
            (bind 'iter (CUndefined))
            (bind '%iter (CUndefined))
            (bind 'next (CUndefined))
            (bind '%next (CUndefined))
            (bind 'FuncIter (CUndefined))
            (bind '%FuncIter (CUndefined))
            (bind 'SeqIter (CUndefined))
            (bind '%SeqIter (CUndefined))
            (bind 'print (CUndefined))
            (bind '%print (CUndefined))
            (bind 'all (CUndefined))
            (bind '%all (CUndefined))
            (bind 'any (CUndefined))
            (bind '%any (CUndefined))
            (bind 'range (CUndefined))
            (bind '%range (CUndefined))
            (bind 'range_iterator (CUndefined))
            (bind '%range_iterator (CUndefined))
            (bind 'filter (CUndefined))
            (bind '%filter (CUndefined))
            (bind 'isinstance (CUndefined))
            (bind '%isinstance (CUndefined))
            (bind 'tuple (CUndefined))
            (bind '%tuple (CUndefined))
            (bind 'issubclass (CUndefined))
            (bind '%issubclass (CUndefined))
            (bind 'bool (CUndefined))
            (bind '%bool (CUndefined))
            (bind 'str (CUndefined))
            (bind '%str (CUndefined))
            (bind 'list (CUndefined))
            (bind '%list (CUndefined))
            (bind 'dict (CUndefined))
            (bind '%dict (CUndefined))
            (bind 'set (CUndefined))
            (bind '%set (CUndefined))
            (bind 'type (CUndefined))
            (bind '%type (CUndefined))
            (bind 'super (CUndefined))
            (bind 'method (CUndefined))
            (bind 'open (CUndefined))
            (bind '%open (CUndefined))
            (bind 'file (CUndefined))
            (bind '%file (CUndefined))
            (bind 'classmethod (CUndefined))
            (bind 'staticmethod (CUndefined))
            (bind '__import__ (CUndefined))
            ;; test functions defined in py-prelude.py
            (bind '___assertEqual (CUndefined))
            (bind '___assertTrue (CUndefined))
            (bind '___assertFalse (CUndefined))
            (bind '___assertIs (CUndefined))
            (bind '___assertIsNot (CUndefined))
            (bind '___assertIn (CUndefined))
            (bind '___assertNotIn (CUndefined))
            (bind '___fail (CUndefined))
            (bind '___assertRaises (CUndefined)))
      ;; dummies of built-in modules
      (map (lambda (name) (bind name (CUndefined)))
           (get-builtin-module-names))
      empty empty))

