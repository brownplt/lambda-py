#lang plai-typed/untyped

;; method - the bound method object, includes classmethod and staticmethod.

(require "../python-core-syntax.rkt" 
         "../util.rkt")

;; mk-method: creates a method object with func and self attributes,
;; it is special-cased to avoid circulariy in object creation.
;; NB: this function actually belongs to the interpreter but, we could
;; get it out when we implement descriptors. Alejandro.
(define (mk-method [w_func : Address] [self : CVal]
                   [env : Env] [sto : Store]) : (CVal * Store)
  (local ([define w_self (new-loc)]
          [define self_sto (hash-set sto w_self self)]
          [define cls (fetch-once (some-v (lookup '%method env)) sto)])
    (values
     (VObjectClass 'method
              (none)
              (hash 
               (list [values '__func__ w_func]
                     [values '__self__ w_self]))
              (some cls))
     self_sto)))
