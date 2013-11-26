#lang plai-typed/untyped

(require "python-core-syntax.rkt")

(define (count-option [node : (optionof CExpr)]) : number
  (type-case (optionof CExpr) node
    [none () 1]
    [some (v) (count v)]))

(define (count node) : number
  (type-case CExpr node
    [CObject (class bval)
             (+ 1 (count class) 1)]
    [CGetAttr (value attr)
              (+ 1 (count value) (count attr))]
    [CSetAttr (obj attr value)
              (+ 1 (count obj) (count attr) (count value))]
    [CSeq (e1 e2)
          (+ 1 (count e1) (count e2))]
    [CAssign (target value)
             (+ 1 (count target) (count value))]
    [CIf (test then els)
         (+ 1 (count test) (count then) (count els))]
    [CLet (x type bind body)
          (+ 2 (count bind) (count body))]
    [CApp (func args starargs)
          (+ 1 (count func) (foldl + 1 (map count args)) (count-option starargs))]
    [CFunc (args varargs body opt-class)
           (+ 3 (count body))]
    [CWhile (test body orelse)
            (+ 1 (count test) (count body) (count orelse))]
    [CReturn (value)
             (+ 1 (count value))]
    [CBuiltinPrim (op args)
                  (+ 1 (foldl + 1 (map count args)))]
    [CList (class values)
           (+ 1 (count class) (foldl + 1 (map count values)))]
    [CTuple (class values)
            (+ 1 (count class) (foldl + 1 (map count values)))]
    [CSet (class values)
          (+ 1 (count class) (foldl + 1 (map count values)))]
    [CRaise (expr)
            (+ 1 (count-option expr))]
    [CTryExceptElse (try _ excepts orelse)
                    (+ 2 (count try) (count excepts) (count orelse))]
    [CTryFinally (try finally)
                 (+ 1 (count try) (count finally))]
    [CYield (expr)
            (+ 1 (count expr))]
    [CModule (prelude body)
             (+ 1 (count prelude) (count body))]
    [CConstructModule (source)
                      (+ 1 (count source))]
    [else 1]))

