#lang plai-typed/untyped

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

(define (set-list (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaSet?
    (some (VObjectClass 'list (some (MetaList (set->list (MetaSet-elts mval1))))
             (hash empty)
             (some (second args))))))

(define (set-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaSet?
               (some (VObjectClass 'num
                              (some (MetaNum (length (set->list (MetaSet-elts mval1)))))
                              (hash empty)
                              (some (second args))))))

(define (set-eq (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (begin
    ;(display args) (display "\n\n")
   (check-types-pred args env sto MetaSet? MetaSet?
               (let ([self (MetaSet-elts mval1)]
                     [other (MetaSet-elts mval2)])
                 (if (set=? self other)
                     (some true-val)
                     (some false-val))))))

(define (set-in [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaSet?
               (let ([contents (MetaSet-elts mval1)])
                 (if (set-member? contents (second args)) ; FIXME: what if (second args) DNE?
                     (some true-val)
                     (some false-val)))))

(define (set-sub (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaSet? MetaSet?
               (let ([self (MetaSet-elts mval1)]
                     [other (MetaSet-elts mval2)])
                    (some (VObjectClass 'set
                                   (some (MetaSet (set-subtract self other)))
                                   (hash empty)
                                   (some (third args)))))))

(define (set-and (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaSet? MetaSet?
               (let ([self (MetaSet-elts mval1)]
                     [other (MetaSet-elts mval2)])
                    (some (VObjectClass 'set
                                   (some (MetaSet (set-intersect self other)))
                                   (hash empty)
                                   (some (third args)))))))

(define (set-or (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaSet? MetaSet?
               (let ([self (MetaSet-elts mval1)]
                     [other (MetaSet-elts mval2)])
                    (some (VObjectClass 'set
                                   (some (MetaSet (set-union self other)))
                                   (hash empty)
                                   (some (third args)))))))

(define (set-xor (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaSet? MetaSet?
               (let ([self (MetaSet-elts mval1)]
                     [other (MetaSet-elts mval2)])
                    (some (VObjectClass 'set
                                   (some (MetaSet (set-symmetric-difference self other)))
                                   (hash empty)
                                   (some (third args)))))))
