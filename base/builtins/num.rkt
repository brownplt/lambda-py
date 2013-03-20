#lang plai-typed/untyped

(require "../python-core-syntax.rkt")
(require "../util.rkt"
         "type.rkt"
         (typed-in racket/base (exact? : (number -> boolean))))

(define (make-builtin-numv [n : number]) : CVal
  (VObject
    (if (exact? n)
      'int
      'float)
    (some (MetaNum n))
    (make-hash empty)))

(define int-class
  
  (seq-ops (list
             (CAssign (CId 'int (GlobalId))
                      (builtin-class
                        'int
                        (list 'num)
                        (CNone)))

             (def 'int '__new__
                  (CFunc (list 'self) (some 'args)
                        (CIf (CBuiltinPrim 'num= (list (py-len 'args) (py-num 0)))
                             (CReturn (py-num 0))
                             (CReturn (py-app (py-getfield (py-getitem 'args 0) '__int__)
                                            (list)
                                            (none))))
                        (some 'int)))
             
             (def 'int '__init__
                  (CFunc (list 'self) (some 'args)
                        (CNone)
                        (some 'int)))

             (def 'int '__bool__
                  (CFunc (list 'self) (none)
                         (CIf (CBuiltinPrim 'num= (list (Id 'self) (py-num 0)))
                              (CReturn (gid 'False))
                              (CReturn (gid 'True)))
                         (some 'int))))))

(define float-class
  (seq-ops (list
             (CAssign (CId 'float (GlobalId))
                      (builtin-class
                        'float
                        (list 'num)
                        (CNone)))
             (def 'float '__new__
                  (CFunc (list 'self) (some 'args)
                        (CIf (CBuiltinPrim 'num= (list (py-len 'args) (py-num 0)))
                             (CReturn (py-num 0))
                             (CReturn (py-app (py-getfield (py-getitem 'args 0) '__float__)
                                            (list)
                                            (none))))
                         (some 'float)))
             (def 'float '__init__
                  (CFunc (list 'self 'other) (none)
                         (CNone)
                         (some 'float)))
             (def 'float '__bool__
                  (CFunc (list 'self) (none)
                         (CIf (CBuiltinPrim 'num= (list (Id 'self) (py-num 0.0)))
                              (CReturn (gid 'False))
                              (CReturn (gid 'True)))
                         (some 'float))))))

(define num-class 
  (seq-ops (list 
             (CAssign (CId 'num (GlobalId))
                      (builtin-class
                        'num
                        (list 'object)
                        (CNone)))
             
             (def 'num '__add__ 
                  (CFunc (list 'self 'other)  (none)
                         (CReturn (CBuiltinPrim 'num+ 
                                                (list 
                                                  (CId 'self (LocalId)) 
                                                  (CId 'other (LocalId)))))
                         (some 'num)))
             
             (def 'num '__sub__ 
                  (CFunc (list 'self 'other)  (none)
                         (CReturn (CBuiltinPrim 'num-
                                                (list 
                                                  (CId 'self (LocalId)) 
                                                  (CId 'other (LocalId)))))
                         (some 'num)))

             (def 'num '__mult__ 
                  (CFunc (list 'self 'other)  (none)
                         (CReturn (CBuiltinPrim 'num* 
                                                (list 
                                                  (CId 'self (LocalId)) 
                                                  (CId 'other (LocalId)))))
                         (some 'num)))
             
             (def 'num '__div__ 
                  (CFunc (list 'self 'other)  (none)
                         (CIf (py-app (py-getfield (CId 'other (LocalId)) '__eq__) 
                                    (list (make-builtin-num 0))
                                    (none))
;
                              (CRaise (some (make-exception 'ZeroDivisionError
                                                            "Divided by 0")))
                              (CReturn (CBuiltinPrim 'num/
                                                     (list 
                                                      (CId 'self (LocalId)) 
                                                      (CId 'other (LocalId))))))
                         (some 'num)))
             
             (def 'num '__floordiv__ 
                  (CFunc (list 'self 'other)  (none)
                         (CIf (py-app (py-getfield (CId 'other (LocalId)) '__eq__) 
                                    (list (make-builtin-num 0))
                                    (none))
                              (CRaise (some (make-exception 'ZeroDivisionError
                                                            "Divided by 0")))
                              (CReturn (CBuiltinPrim 'num//
                                                     (list 
                                                       (CId 'self (LocalId)) 
                                                       (CId 'other (LocalId))))))
                         (some 'num)))
             
             (def 'num '__mod__ 
                  (CFunc (list 'self 'other)  (none)
                         (CIf (py-app (py-getfield (CId 'other (LocalId)) '__eq__) 
                                    (list (make-builtin-num 0))
                                    (none))
                              (CRaise (some (make-exception 'ZeroDivisionError
                                                            "Divided by 0")))
                              (CReturn (CBuiltinPrim 'num%
                                                     (list 
                                                       (CId 'self (LocalId)) 
                                                       (CId 'other (LocalId))))))
                         (some 'num)))
             
             (def 'num '__str__
                  (CFunc (list 'self) (none)
                         (CReturn (CBuiltinPrim 'num-str
                                                (list (CId 'self (LocalId)))))
                         (some 'num)))
             
             (def 'num '__eq__
                  (CFunc (list 'self 'other) (none)
                         (CReturn (CBuiltinPrim 'num=
                                                (list
                                                  (CId 'self (LocalId))
                                                  (CId 'other (LocalId)))))
                         (some 'num)))
             
             (def 'num '__gt__
                  (CFunc (list 'self 'other) (none)
                         (CReturn (CBuiltinPrim 'num>
                                                (list
                                                  (CId 'self (LocalId))
                                                  (CId 'other (LocalId)))))
                         (some 'num)))
             
             (def 'num '__lt__
                  (CFunc (list 'self 'other) (none)
                         (CReturn (CBuiltinPrim 'num<
                                                (list
                                                  (CId 'self (LocalId))
                                                  (CId 'other (LocalId)))))
                         (some 'num)))
             
             (def 'num '__gte__
                  (CFunc (list 'self 'other) (none)
                         (CReturn (CBuiltinPrim 'num>=
                                                (list
                                                  (CId 'self (LocalId))
                                                  (CId 'other (LocalId)))))
                         (some 'num)))
             
             (def 'num '__invrt__
                  (CFunc  (list 'self) (none)
                          (CReturn (CBuiltinPrim 'num-
                                                 (list 
                                                  (make-builtin-num 0) 
                                                  (CBuiltinPrim 'num+
                                                                (list (CId 'self (LocalId)) 
                                                                      (make-builtin-num
                                                                       1))))))
                          (some 'num)))
             
             (def 'num '__abs__
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
             
             (def 'num '__lte__
                  (CFunc (list 'self 'other) (none)
                         (CReturn (CBuiltinPrim 'num<=
                                                (list
                                                  (CId 'self (LocalId))
                                                  (CId 'other (LocalId)))))
                         (some 'num)))
             (def 'num '__hash__
                  (CFunc (list 'self) (none)
                         (CReturn (CId 'self (LocalId)))
                         (some 'num)))
             (def 'num '__cmp__
                  (CFunc (list 'self 'other) (none)
                         (CReturn (CBuiltinPrim 'numcmp
                                                (list
                                                  (CId 'self (LocalId))
                                                  (CId 'other (LocalId)))))
                         (some 'num))))))

