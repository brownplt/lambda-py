#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt"
         (typed-in racket/base (exact? : (number -> boolean))))

(define (make-builtin-num [n : number]) : CExpr
  (CObject
    (if (exact? n)
      'int
      'float)
    (some (MetaNum n))))

(define (make-builtin-numv [n : number]) : CVal
  (VObject
    (if (exact? n)
      'int
      'float)
    (some (MetaNum n))
    (make-hash empty)))

(define int-class
  (CClass
    'int
    'num

    (seq-ops (list
               (def '__init__
                    (CFunc (list 'self 'other) (none)
                        (CAssign (CId 'self (LocalId))
                            (CApp (CGetField (CId 'other (LocalId)) '__int__)
                                  (list (CId 'other (LocalId)))
                                  (none)))
                        true))))))

(define float-class
  (CClass
    'int
    'num
    (seq-ops (list
               (def '__init__
                    (CFunc (list 'self 'other) (none)
                        (CAssign (CId 'self (LocalId))
                            (CApp (CGetField (CId 'other (LocalId)) '__float__)
                                  (list (CId 'other (LocalId)))
                                  (none)))
                        true))))))

(define num-class 
  (CClass
    'num
    'object
    (seq-ops (list 
               (def '__add__ 
                    (CFunc (list 'self 'other)  (none)
                           (CReturn (CBuiltinPrim 'num+ 
                                         (list 
                                           (CId 'self (LocalId)) 
                                           (CId 'other (LocalId)))))
                           true))
               (def '__sub__ 
                    (CFunc (list 'self 'other)  (none)
                           (CReturn (CBuiltinPrim 'num-
                                         (list 
                                           (CId 'self (LocalId)) 
                                           (CId 'other (LocalId)))))
                           true))

               (def '__mult__ 
                    (CFunc (list 'self 'other)  (none)
                           (CReturn (CBuiltinPrim 'num* 
                                         (list 
                                           (CId 'self (LocalId)) 
                                           (CId 'other (LocalId)))))
                           true))
               (def '__div__ 
                    (CFunc (list 'self 'other)  (none)
                           (CIf (CApp (CGetField (CId 'other (LocalId)) '__eq__) 
                                       (list (CId 'other (LocalId)) (make-builtin-num 0))
                                       (none))
                                (CRaise (some (make-exception 'ZeroDivisionError
                                                              "Divided by 0")))
                                (CReturn (CBuiltinPrim 'num/
                                              (list 
                                                (CId 'self (LocalId)) 
                                                (CId 'other (LocalId))))))
                           true))
               (def '__floordiv__ 
                    (CFunc (list 'self 'other)  (none)
                           (CIf (CApp (CGetField (CId 'other (LocalId)) '__eq__) 
                                       (list (CId 'other (LocalId)) (make-builtin-num 0))
                                       (none))
                                (CRaise (some (make-exception 'ZeroDivisionError
                                                              "Divided by 0")))
                                (CReturn (CBuiltinPrim 'num//
                                              (list 
                                                (CId 'self (LocalId)) 
                                                (CId 'other (LocalId))))))
                           true))
               (def '__mod__ 
                    (CFunc (list 'self 'other)  (none)
                           (CIf (CApp (CGetField (CId 'other (LocalId)) '__eq__) 
                                       (list (CId 'other (LocalId)) (make-builtin-num 0))
                                       (none))
                                (CRaise (some (make-exception 'ZeroDivisionError
                                                              "Divided by 0")))
                                (CReturn (CBuiltinPrim 'num%
                                              (list 
                                                (CId 'self (LocalId)) 
                                                (CId 'other (LocalId))))))
                           true))
               (def '__str__
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'num-str
                                        (list (CId 'self (LocalId)))))
                          true))
               (def '__eq__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num=
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))
                           true))
               (def '__gt__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num>
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))
                           true))
               (def '__lt__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num<
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))
                           true))
               (def '__gte__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num>=
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))
                           true))
               (def '__invrt__
                    (CFunc  (list 'self) (none)
                      (CReturn (CBuiltinPrim 'num-
                                             (list 
                                               (make-builtin-num 0) 
                                               (CBuiltinPrim 'num+
                                                   (list (CId 'self (LocalId)) 
                                                         (make-builtin-num
                                                           1))))))
                      true))
               (def '__abs__
                    (CFunc (list 'self) (none)
                           (CIf (CBuiltinPrim 'num< 
                                              (list 
                                                (CId 'self (LocalId))
                                                (make-builtin-num 0)))
                                (CReturn (CBuiltinPrim 'num-
                                                       (list (make-builtin-num 0)
                                                           (CId 'self (LocalId)))))
                                (CReturn (CBuiltinPrim 'num+
                                                       (list (make-builtin-num 0)
                                                           (CId 'self (LocalId))))))
                           true))
               (def '__lte__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num<=
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))
                           true))
               (def '__cmp__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'numcmp
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))
                           true))))))



