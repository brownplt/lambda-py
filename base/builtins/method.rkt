#lang plai-typed/untyped

;; method - the bound method object, includes classmethod and staticmethod.

(require "../python-core-syntax.rkt" 
         "../util.rkt")

;; mk-method: creates a method object with func and self attributes,
;; it is special-cased to avoid circulariy in object creation.
;; The optional address parameter is used to support self aliasing
;; NB: this function actually belongs to the interpreter and will be simpler with
;; the new addresses. Alejandro.
(define (mk-method [w_func : Address] [self : CVal] [opt_w_self : (optionof Address)]
                   [sto : Store]) : (CVal * Store)
  (local ([define w_self (if (some? opt_w_self) (some-v opt_w_self) (new-loc))]
          [define self_sto (if (none? opt_w_self) (hash-set sto w_self self) sto)])
    (values
     (VObjectClass 'method
              (none)
              (hash 
               (list [values '__func__ w_func]
                     [values '__self__ w_self]))
              (none))
     self_sto)))
