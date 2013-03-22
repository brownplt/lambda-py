#lang plai-typed/untyped

(require "../python-core-syntax.rkt")
(require "../util.rkt")
(require [opaque-type-in racket/set [Set set?]])
(require (typed-in racket/set (set->list : (set? -> (listof 'a)))))

(define (set-list (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaSet?
    (some (VObjectClass 'list (some (MetaList (set->list (MetaSet-elts mval1))))
             (hash empty)
             (some (second args))))))

(define (set-str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaSet?
    (some (VObjectClass 'str (some (MetaStr (pretty-metaval mval1 sto)))
                        (hash empty) (some (second args))))))

(define (set-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaSet?
               (some (VObjectClass 'int
                              (some (MetaNum (length (set->list (MetaSet-elts mval1)))))
                              (hash empty)
                              (some (second args))))))

