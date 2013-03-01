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
         "builtins/method.rkt"
         "util.rkt"
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

(define exception
  (seq-ops
    (list 
      (CAssign (CId 'Exception (GlobalId))
               (CClass
                 'Exception
                 (list 'object)
                 (CNone)))
      (def 'Exception '__init__
           (CFunc (list 'self) (some 'args)
                  (CAssign
                   (CGetField
                    (CId 'self (LocalId))
                    'args)
                   (CId 'args (LocalId)))
                  (some 'Exception)))
      (def 'Exception '__str__
           (CFunc (list 'self) (none)
                  (CReturn
                    (CBuiltinPrim 'exception-str
                                  (list (CId 'self (LocalId)))))
                  (some 'Exception))))))

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

(define iter-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self (LocalId))
          '__iter__)
        (list)
        (none)))
    (none)))

(define next-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self (LocalId))
          '__next__)
        (list)
        (none)))
    (none)))

(define isinstance-lambda
  (CFunc (list 'self 'type) (none)
    (CReturn
      (CBuiltinPrim 'isinstance
                    (list (CId 'self (LocalId))
                          (CId 'type (LocalId)))))
    (none)))

(define print-lambda
  (CFunc (list 'to-print) (none) 
         (CSeq 
          (CPrim1 'print (CApp 
                          (CGetField (CId 'to-print (LocalId)) '__str__) 
                          (list)
                          (none)))
          (CNone))
         (none)))

(define callable-lambda
  (CFunc (list 'to-check) (none)
         (CSeq
          (CTryExceptElseFinally
           ;; try to get __call__ attribute and return True
           (CSeq
            (CGetField (CId 'to-check (LocalId)) '__call__)
            (CReturn (CTrue)))
           (list (CExcept (list) (none) (CNone))) (CNone) (CNone))
          ;; use the primary operator
          (CReturn (CPrim1 'callable (CId 'to-check (LocalId)))))
         (none)))

(define locals-lambda
  (CFunc (list) (none)
         (CReturn
          (CBuiltinPrim '$locals empty))
         (none)))
  
(define lib-functions
  (list (bind 'True (assign 'True (CTrue)))
        (bind 'False (assign 'False (CFalse)))
        (bind 'None (assign 'None (CNone)))

        (bind 'object object-class)
        (bind 'none none-class)
        (bind 'num num-class)
        (bind '%num (assign '%num (CId 'num (GlobalId))))
        (bind 'int int-class)
        (bind '%int (assign '%int (CId 'int (GlobalId))))
        (bind 'float float-class)
        (bind '%float (assign '%float (CId 'float (GlobalId))))
        (bind 'str str-class)
        (bind '%str (assign '%str (CId 'str (GlobalId))))
        (bind 'bool bool-class)
        (bind 'file file-class)
        (bind 'open file-class)
        (bind 'method method-class)
        (bind 'classmethod classmethod-class)
        (bind 'staticmethod staticmethod-class)
        (bind 'super super-class)

        (bind 'len (assign 'len len-lambda))
        (bind 'min (assign 'min min-lambda))
        (bind 'max (assign 'max max-lambda))
        (bind 'next (assign 'next next-lambda))
        (bind 'abs (assign 'abs abs-lambda))

        (bind 'isinstance (assign 'isinstance isinstance-lambda))
        (bind 'print (assign 'print print-lambda))
        (bind 'callable (assign 'callable callable-lambda))
        (bind 'locals (assign 'locals locals-lambda))

        (bind 'Exception exception)
        (bind 'NameError (assign 'NameError (make-exception-class 'NameError)))
        (bind 'TypeError (assign 'TypeError (make-exception-class 'TypeError)))
        (bind 'ValueError (assign 'ValueError (make-exception-class 'ValueError)))
        (bind 'SyntaxError (assign 'SyntaxError (make-exception-class 'SyntaxError)))
        (bind 'AttributeError (assign 'AttributeError (make-exception-class 'AttributeError)))
        (bind 'RuntimeError (assign 'RuntimeError (make-exception-class 'RuntimeError)))
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
            (bind 'iter (CUndefined))
            (bind 'FuncIter (CUndefined))
            (bind 'SeqIter (CUndefined))
            (bind 'all (CUndefined))
            (bind 'any (CUndefined))
            (bind 'range (CUndefined))
            (bind 'filter (CUndefined))
            (bind 'dicteq (CUndefined))
            (bind 'tuple (CUndefined))
            (bind '%tuple (CUndefined))
            (bind 'list (CUndefined))
            (bind '%list (CUndefined))
            (bind 'dict (CUndefined))
            (bind '%dict (CUndefined))
            (bind 'set (CUndefined))
            (bind '%set (CUndefined))
            (bind 'type (CUndefined))
            (bind '%type (CUndefined))
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
      empty empty empty))

