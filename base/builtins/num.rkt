#lang plai-typed/untyped

(require "../python-core-syntax.rkt")
(require "../util.rkt"
         "type.rkt"
         (typed-in racket
                   [bitwise-and : (number number -> number)]
                   [bitwise-ior : (number number -> number)]
                   [bitwise-xor : (number number -> number)]
                   [arithmetic-shift : (number number -> number)]
                   [inexact->exact : (number -> number)]
                   [truncate : (number -> number)]
                   [exact? : (number -> boolean)]))

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
                             (CReturn (make-builtin-num 0))
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
                         (some 'int)))

             (def 'int '__and__
                  (CFunc (list 'self 'other)  (none)
                         (CReturn (CBuiltinPrim 'int-and
                                                (list
                                                 (CId 'self (LocalId))
                                                 (CId 'other (LocalId)))))
                         (some 'int)))

             (def 'int '__or__
                  (CFunc (list 'self 'other)  (none)
                         (CReturn (CBuiltinPrim 'int-or
                                                (list
                                                 (CId 'self (LocalId))
                                                 (CId 'other (LocalId)))))
                         (some 'int)))

             (def 'int '__xor__
                  (CFunc (list 'self 'other)  (none)
                         (CReturn (CBuiltinPrim 'int-xor
                                                (list
                                                 (CId 'self (LocalId))
                                                 (CId 'other (LocalId)))))
                         (some 'int)))

             (def 'int '__lshift__
                  (CFunc (list 'self 'other)  (none)
                         (CReturn (CBuiltinPrim 'int-shift
                                                (list
                                                 (CId 'self (LocalId))
                                                 (CId 'other (LocalId)))))
                         (some 'int)))

             (def 'int '__rshift__
                  (CFunc (list 'self 'other)  (none)
                         (CReturn (CBuiltinPrim 'int-shift
                                                (list
                                                 (CId 'self (LocalId))
                                                 (CBuiltinPrim 'num-
                                                               (list
                                                                (make-builtin-num 0)
                                                                (CId 'other (LocalId)))))))
                         (some 'int)))

             (def 'int '__invrt__
                  (CFunc  (list 'self) (none)
                          (CReturn (CBuiltinPrim 'num-
                                                 (list
                                                  (make-builtin-num 0)
                                                  (CBuiltinPrim 'num+
                                                                (list (CId 'self (LocalId))
                                                                      (make-builtin-num
                                                                       1))))))
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
                             (CReturn (make-builtin-num 0.0))
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
             
             (def 'num '__pow__
                  (CFunc (list 'self 'other)  (none)
                         (CReturn (CBuiltinPrim 'num**
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
                                                (list (CId 'self (LocalId)) (CId '%str (GlobalId)))))
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
                         (some 'num)))
             (def 'num '__int__
                  (CFunc (list 'self) (none)
                         (CReturn (CBuiltinPrim 'int
                                                (list
                                                 (CId 'self (LocalId)))))
                         (some 'num)))
             (def 'num '__float__
                  (CFunc (list 'self) (none)
                         (CReturn (CBuiltinPrim 'num+
                                                (list
                                                 (CId 'self (LocalId))
                                                 (make-builtin-num 0.0))))
                         (some 'num))))))

(define (int args env sto)
  (check-types-pred args env sto MetaNum?
                    (some (make-builtin-numv (inexact->exact (truncate (MetaNum-n mval1)))))))

(define (int-and args env sto)
  (check-types-pred args env sto MetaNum? MetaNum?
                        (some (make-builtin-numv (bitwise-and (MetaNum-n mval1)
                                                              (MetaNum-n mval2))))))

(define (int-or args env sto)
  (check-types-pred args env sto MetaNum? MetaNum?
                        (some (make-builtin-numv (bitwise-ior (MetaNum-n mval1)
                                                              (MetaNum-n mval2))))))

(define (int-xor args env sto)
  (check-types-pred args env sto MetaNum? MetaNum?
                        (some (make-builtin-numv (bitwise-xor (MetaNum-n mval1)
                                                              (MetaNum-n mval2))))))

(define (int-shift args env sto)
  (check-types-pred args env sto MetaNum? MetaNum?
                    (some (make-builtin-numv (arithmetic-shift (MetaNum-n mval1)
                                                                   (MetaNum-n mval2))))))
