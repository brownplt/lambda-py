#lang plai-typed

(require "python-core-syntax.rkt")
(require "builtins/num.rkt"
         "builtins/str.rkt"
         "builtins/list.rkt"
         "builtins/tuple.rkt"
         "builtins/dict.rkt"
         "builtins/object.rkt"
         "builtins/bool.rkt"
         "builtins/set.rkt"
         "builtins/none.rkt"
         "util.rkt"
         (typed-in "get-structured-python.rkt"
                   (get-structured-python : ('a -> 'b)))
         (typed-in "parse-python.rkt"
                   (parse-python/port : ('a string -> 'b)))
         (typed-in racket/base (open-input-file : ('a -> 'b)))
         "python-syntax.rkt"
         "python-desugar.rkt"
         (typed-in racket/base (append : ((listof 'a) (listof 'a) (listof 'a) (listof 'a) (listof 'a) -> (listof 'a)))))

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#


(define print-lambda
  (CFunc (list 'to-print) (none) 
    (CSeq 
      (CPrim1 'print (CApp 
                       (CGetField (CId 'to-print (LocalId)) '__str__) 
                       (list (CId 'to-print (LocalId)))
                       (none)))
      (CNone))
    false))

(define callable-lambda
  (CFunc (list 'to-check) (none)
      (CReturn
        (CPrim1 'callable (CId 'to-check (LocalId))))
      false))


(define assert-true-lambda
  (CFunc (list 'check-true) (none)
    (CIf (CId 'check-true (LocalId)) (CNone) (CError (CStr "Assert failed")))
    false))

(define assert-false-lambda
  (CFunc (list 'check-false) (none)
    (CIf (CId 'check-false (LocalId)) (CError (CStr "Assert failed")) (CTrue))
    false))

(define assert-equal-lambda
  (CFunc (list 'check1 'check2)  (none)
    (CIf (CApp (CGetField (CId 'check1 (LocalId)) '__eq__)
               (list (CId 'check1 (LocalId)) (CId 'check2 (LocalId)))
               (none))
         (CNone)
         (CError (CStr "Assert failed")))
    false))

(define assert-is-lambda
  (CFunc (list 'check1 'check2) (none)
    (CIf (CPrim2 'Is (CId 'check1 (LocalId)) (CId 'check2 (LocalId)))
         (CNone)
         (CError (CStr "Assert failed")))
    false))

(define assert-isnot-lambda
  (CFunc (list 'check1 'check2) (none)
    (CIf (CPrim2 'Is (CId 'check1 (LocalId)) (CId 'check2 (LocalId)))
         (CError (CStr "Assert failed"))
         (CNone))
    false))

(define assert-in-lambda
  (CFunc (list 'check1 'check2) (none)
    (CIf (desugar (PyBinOp (PyId 'check1 'DUMMY) 'In (PyId 'check2 'DUMMY)))
         (CNone)
         (CError (CStr "Assert failed")))
    false))

(define assert-notin-lambda
  (CFunc (list 'check1 'check2) (none)
    (CIf (desugar (PyBinOp (PyId 'check1 'DUMMY) 'In (PyId 'check2 'DUMMY)))
         (CError (CStr "Assert failed"))
         (CNone))
    false))

(define fail-lambda
  (CFunc (list 'arg) (none)
    (CError (CId 'arg (LocalId)))
    false))

(define exception
  (CClass
    'Exception
    'object
    (seq-ops (list 
               (def '__init__
                    (CFunc (list 'self) (some 'args)
                           (CSeq 
                             (CAssign 
                               (CGetField
                                 (CId 'self (LocalId))
                                 'args)
                               (CId 'args (LocalId)))
                             (CAssign
                               (CGetField
                                 (CId 'self (LocalId))
                                 '__class__)
                               (CId 'Exception (LocalId))))
                           true))
               (def '__str__
                    (CFunc (list 'self) (none)
                           (CReturn
                               (CBuiltinPrim 'exception-str
                                 (list (CId 'self (LocalId)))))
                           true))))))

(define (make-exception-class [name : symbol]) : CExpr
  (CClass
    name
    'Exception
    (CNone)))

(define len-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self (LocalId))
          '__len__)
        (list (CId 'self (LocalId)))
        (none)))
    false))

(define min-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self (LocalId))
          '__min__)
        (list (CId 'self (LocalId)))
        (none)))
    false))

(define max-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self (LocalId))
          '__max__)
        (list (CId 'self (LocalId)))
        (none)))
    false))

(define abs-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self (LocalId))
          '__abs__)
        (list (CId 'self (LocalId)))
        (none)))
    false))

(define iter-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self (LocalId))
          '__iter__)
        (list (CId 'self (LocalId)))
        (none)))
    false))

(define next-lambda
  (CFunc (list 'self) (none)
    (CReturn
      (CApp
        (CGetField
          (CId 'self (LocalId))
          '__next__)
        (list (CId 'self (LocalId)))
        (none)))
    false))


(define isinstance-lambda
  (CFunc (list 'self 'type) (none)
    (CReturn
      (CBuiltinPrim 'isinstance
                    (list (CId 'self (LocalId))
                          (CId 'type (LocalId)))))
    false))

; Returns the $class of self's $class.
; self's $class is the class of the given instance.
; the $class of that is its antecedent.
; So this function returns the super-class of the instance.
(define super-lambda
  (CFunc (list 'self) (none)
         (CReturn
             (CBuiltinPrim '$super (list
                 (CBuiltinPrim '$super (list (CId 'self (LocalId)))))))
         false))

(define-type LibBinding
  [bind (left : symbol) (right : CExpr)])


(define lib-functions
  (list (bind 'True (CTrue))
        (bind 'False (CFalse))
        (bind 'None (CNone))

        (bind 'object object-class)
        (bind 'none none-class)
        (bind 'num num-class)
        (bind 'int int-class)
        (bind 'float float-class)
        (bind 'str str-class)
        (bind 'list list-class)
        (bind 'tuple tuple-class)
        ; this is a hack because one test overrides the dict name, 
        ; we should do this $ thing for all builtin names for this reason
        (bind '$dict dict-class) 
        (bind 'dict (CId '$dict (GlobalId)))
        (bind 'bool bool-class)
        (bind 'set set-class)
        (bind 'len len-lambda)
        (bind 'min min-lambda)
        (bind 'max max-lambda)
        (bind 'next next-lambda)
        (bind 'abs abs-lambda)
        (bind 'isinstance isinstance-lambda)
        (bind 'print print-lambda)

        (bind 'callable callable-lambda)

        (bind 'super super-lambda)

        (bind 'Exception exception)
        (bind 'NameError (make-exception-class 'NameError))
        (bind 'TypeError (make-exception-class 'TypeError))
        (bind 'ValueError (make-exception-class 'ValueError))
        (bind 'SyntaxError (make-exception-class 'SyntaxError))
        (bind 'AttributeError (make-exception-class 'AttributeError))
        (bind 'RuntimeError (make-exception-class 'RuntimeError))
        (bind 'KeyError (make-exception-class 'KeyError))
        (bind 'IndexError (make-exception-class 'IndexError))
        (bind 'UnboundLocalError (make-exception-class 'UnboundLocalError))
        (bind 'IndexError (make-exception-class 'IndexError))
        (bind 'ZeroDivisionError (make-exception-class 'ZeroDivisionError))
        (bind 'StopIteration (make-exception-class 'StopIteration))
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertFalse assert-false-lambda)
        (bind '___assertIs assert-is-lambda)
        (bind '___assertIsNot assert-isnot-lambda)
        (bind '___assertIn assert-in-lambda)
        (bind '___assertNotIn assert-notin-lambda)
        (bind '___fail fail-lambda)))

(define lib-function-dummies
  (append
      (map (lambda(b) (bind (bind-left b) (CNone)))
           lib-functions)
      (list (bind 'iter (CNone))
            (bind 'FuncIter (CNone))
            (bind 'SeqIter (CNone))
            (bind 'all (CNone))
            (bind 'any (CNone))
            (bind 'range (CNone))
            ;(bind '___assertRaises (CNone))
            (bind 'filter (CNone))
            (bind 'dicteq (CNone)))
      empty empty empty))
;; these are builtin functions that we have written in actual python files which
;; are pulled in here and desugared for lib purposes
(define pylib-programs
  (map (lambda(file) 
         (desugar 
           (get-structured-python 
             (parse-python/port 
               (open-input-file file)
               python-path))))
       (list "pylib/range.py"
             "pylib/seq_iter.py"
             "pylib/filter.py"
             "pylib/any.py"
             "pylib/all.py"
             "pylib/dicteq.py"
            ; "pylib/assertraises.py"
            )))
             

(define-type-alias Lib (CExpr -> CExpr))

(define (python-lib [expr : CExpr]) : CExpr
  (seq-ops (append
             (map (lambda(b) (CAssign (CId (bind-left b) (GlobalId)) (bind-right b)))
                      lib-function-dummies)
             (list (CModule-prelude expr))
             (map (lambda(b) (CAssign (CId (bind-left b) (GlobalId)) (bind-right b)))
                      lib-functions)
             pylib-programs  
             (list (CModule-body expr)))))
