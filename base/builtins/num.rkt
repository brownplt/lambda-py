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
    (list 'num)

    (seq-ops (list
               (def '__init__
                    (CFunc (list 'self 'other) (none)
                        (CAssign (CId 'self (LocalId))
                            (CApp (CGetField (CId 'other (LocalId)) '__int__)
                                  (list)
                                  (none)))
                        (some 'int)))))))

(define float-class
  (CClass
    'float
    (list 'num)
    (seq-ops (list
               (def '__init__
                    (CFunc (list 'self 'other) (none)
                        (CAssign (CId 'self (LocalId))
                            (CApp (CGetField (CId 'other (LocalId)) '__float__)
                                  (list)
                                  (none)))
                        (some 'float)))))))

(define num-class 
  (CClass
    'num
    (list 'object)
    (seq-ops (list 
               (def '__add__ 
                    (CFunc (list 'self 'other)  (none)
                           (CReturn (CBuiltinPrim 'num+ 
                                         (list 
                                           (CId 'self (LocalId)) 
                                           (CId 'other (LocalId)))))
                           (some 'num)))
               (def '__sub__ 
                    (CFunc (list 'self 'other)  (none)
                           (CReturn (CBuiltinPrim 'num-
                                         (list 
                                           (CId 'self (LocalId)) 
                                           (CId 'other (LocalId)))))
                           (some 'num)))

               (def '__mult__ 
                    (CFunc (list 'self 'other)  (none)
                           (CReturn (CBuiltinPrim 'num* 
                                         (list 
                                           (CId 'self (LocalId)) 
                                           (CId 'other (LocalId)))))
                           (some 'num)))
               (def '__div__ 
                    (CFunc (list 'self 'other)  (none)
                           (CIf (CApp (CGetField (CId 'other (LocalId)) '__eq__) 
                                       (list (make-builtin-num 0))
                                       (none))
                                (CRaise (some (make-exception 'ZeroDivisionError
                                                              "Divided by 0")))
                                (CReturn (CBuiltinPrim 'num/
                                              (list 
                                                (CId 'self (LocalId)) 
                                                (CId 'other (LocalId))))))
                           (some 'num)))
               (def '__floordiv__ 
                    (CFunc (list 'self 'other)  (none)
                           (CIf (CApp (CGetField (CId 'other (LocalId)) '__eq__) 
                                       (list (make-builtin-num 0))
                                       (none))
                                (CRaise (some (make-exception 'ZeroDivisionError
                                                              "Divided by 0")))
                                (CReturn (CBuiltinPrim 'num//
                                              (list 
                                                (CId 'self (LocalId)) 
                                                (CId 'other (LocalId))))))
                           (some 'num)))
               (def '__mod__ 
                    (CFunc (list 'self 'other)  (none)
                           (CIf (CApp (CGetField (CId 'other (LocalId)) '__eq__) 
                                       (list (make-builtin-num 0))
                                       (none))
                                (CRaise (some (make-exception 'ZeroDivisionError
                                                              "Divided by 0")))
                                (CReturn (CBuiltinPrim 'num%
                                              (list 
                                                (CId 'self (LocalId)) 
                                                (CId 'other (LocalId))))))
                           (some 'num)))
               (def '__str__
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'num-str
                                        (list (CId 'self (LocalId)))))
                          (some 'num)))
               (def '__eq__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num=
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))
                           (some 'num)))
               (def '__gt__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num>
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))
                           (some 'num)))
               (def '__lt__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num<
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))
                           (some 'num)))
               (def '__gte__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num>=
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))
                           (some 'num)))
               (def '__invrt__
                    (CFunc  (list 'self) (none)
                      (CReturn (CBuiltinPrim 'num-
                                             (list 
                                               (make-builtin-num 0) 
                                               (CBuiltinPrim 'num+
                                                   (list (CId 'self (LocalId)) 
                                                         (make-builtin-num
                                                           1))))))
                      (some 'num)))
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
                           (some 'num)))
               (def '__lte__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'num<=
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))
                           (some 'num)))
               (def '__cmp__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'numcmp
                                                  (list
                                                    (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))
                           (some 'num)))))))



