#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt")
(require [opaque-type-in racket/set [Set set?]])
(require
  (typed-in racket/set (set->list : (set? -> (listof 'a))))
  (typed-in racket/set (set? : ('a -> boolean)))
  (typed-in racket/set (set=? : (set? set? -> boolean)))
  (typed-in racket/set (set-member? : (set? 'a -> boolean)))
  (typed-in racket/set (set-subtract : (set? set? -> set?)))
  (typed-in racket/set (set-intersect : (set? set? -> set?)))
  (typed-in racket/set (set->list : (set? -> (listof 'a))))
  (typed-in racket/set (set-union : (set? set? -> set?)))
  (typed-in racket/set (set-symmetric-difference : (set? set? -> set?)))
)

(define set-class : CExpr
  (CClass
   'set
   'object
   (seq-ops (list 
              (def '__len__
                    (CFunc (list 'self) (none)
                           (CReturn (CBuiltinPrim 'set-len
                                                  (list
                                                   (CId 'self (LocalId)))))
                           true))
              (def '__set__
                    (CFunc (list 'self) (none)
                           (CReturn (CBuiltinPrim 'set-set
                                                  (list
                                                   (CId 'self (LocalId)))))
                           true))
              (def '__init__
                   (CFunc (list 'self) (some 'args)
                          (CReturn
                          (CIf ; Did we get any args?
                            (CBuiltinPrim 'num=
                                          (list
                                            (CApp (CGetField (CId 'args (LocalId)) '__len__)
                                                  (list (CId 'args (LocalId)))
                                                  (none))
                                            (CObject 'num (some (MetaNum 0)))))
                            ; No. Return an empty set
                            (CSet empty)
                            ; Yes. Call __set__ on the first argument.
                            (CLet 'first-arg
                                  (CApp (CGetField (CId 'args (LocalId)) '__getitem__)
                                        (list (CId 'args (LocalId))
                                              (CObject 'num (some (MetaNum 0))))
                                        (none))
                                  (CApp (CGetField (CId 'first-arg (LocalId)) '__set__)
                                        (list (CId 'first-arg (LocalId)))
                                        (none)))))
                          true)
                          )

                          ;(CReturn (CBuiltinPrim 'set-init
                           ;                          (list (CId 'self))))))
              #|
              (def 'clear
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'set-clear
                                                     (list (CId 'self))))
                          true))

              (def 'update
                   (CFunc (list 'self 'other) (none)
                          (CReturn (CBuiltinPrim 'set-update
                                                     (list (CId 'self)
                                                           (CId 'other))))
                          true))
              |#

              (def '__iter__
                   (CFunc (list 'self) (none)
                       (CReturn (CApp (CGetField (CId 'SeqIter (LocalId)) '__init__)
                                      (list (CObject 'SeqIter (none)) 
                                            (CApp (CGetField (CId 'self
                                                                  (LocalId))
                                                             '__list__)
                                                  (list (CId 'self
                                                             (LocalId)))
                                                  (none))) 
                                      (none)))
                       true))
              (def '__in__
                (CFunc (list 'self 'other) (none)
                       (CReturn (CBuiltinPrim 'set-in
                                              (list
                                               (CId 'self (LocalId))
                                               (CId 'other (LocalId))
                                               )))
                       true))

              (def '__eq__
                (CFunc (list 'self 'other) (none)
                       (CReturn (CBuiltinPrim 'set-eq
                                              (list
                                               (CId 'self (LocalId))
                                               (CId 'other (LocalId))
                                               )))
                       true))

              (def '__sub__
                (CFunc (list 'self 'other) (none)
                       (CReturn (CBuiltinPrim 'set-sub
                                              (list (CId 'self (LocalId)) 
                                                    (CId 'other (LocalId)))))
                       true))
              (def '__and__
                (CFunc (list 'self 'other) (none)
                       (CReturn (CBuiltinPrim 'set-and
                                              (list (CId 'self (LocalId)) 
                                                    (CId 'other (LocalId)))))
                       true))

              (def '__or__
                (CFunc (list 'self 'other) (none)
                       (CReturn (CBuiltinPrim 'set-or
                                              (list (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))
                       true))

              (def '__xor__
                (CFunc (list 'self 'other) (none)
                       (CReturn (CBuiltinPrim 'set-xor
                                              (list (CId 'self (LocalId))
                                                    (CId 'other (LocalId)))))
                       true))
              (def '__list__
                (CFunc (list 'self) (none)
                       (CReturn (CBuiltinPrim 'set-list
                                              (list (CId 'self (LocalId)))))
                       true))
))))

; returns a copy of this set
(define (set-set (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'set
               (let ([elts (MetaSet-elts mval1)])
                    (some (VObject 'set
                                   (some (MetaSet elts))
                                   (hash empty))))))

(define (set-list (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'set
    (some (VObject 'list (some (MetaList (set->list (MetaSet-elts mval1))))
             (hash empty)))))

(define (set-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'set
               (some (VObject 'num
                              (some (MetaNum (length (set->list (MetaSet-elts mval1)))))
                              (hash empty)))))

(define (set-eq (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (begin
    ;(display args) (display "\n\n")
   (check-types args env sto 'set 'set
               (let ([self (MetaSet-elts mval1)]
                     [other (MetaSet-elts mval2)])
                 (if (set=? self other)
                     (some true-val)
                     (some false-val))))))

(define (set-in [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'set
               (let ([contents (MetaSet-elts mval1)])
                 (if (set-member? contents (second args)) ; FIXME: what if (second args) DNE?
                     (some true-val)
                     (some false-val)))))

(define (set-sub (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'set 'set
               (let ([self (MetaSet-elts mval1)]
                     [other (MetaSet-elts mval2)])
                    (some (VObject 'set
                                   (some (MetaSet (set-subtract self other)))
                                   (hash empty))))))

(define (set-and (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'set 'set
               (let ([self (MetaSet-elts mval1)]
                     [other (MetaSet-elts mval2)])
                    (some (VObject 'set
                                   (some (MetaSet (set-intersect self other)))
                                   (hash empty))))))

(define (set-or (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'set 'set
               (let ([self (MetaSet-elts mval1)]
                     [other (MetaSet-elts mval2)])
                    (some (VObject 'set
                                   (some (MetaSet (set-union self other)))
                                   (hash empty))))))

(define (set-xor (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'set 'set
               (let ([self (MetaSet-elts mval1)]
                     [other (MetaSet-elts mval2)])
                    (some (VObject 'set
                                   (some (MetaSet (set-symmetric-difference self other)))
                                   (hash empty))))))
