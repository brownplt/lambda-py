#lang plai-typed

(require "../python-core-syntax.rkt"
         "../util.rkt")

; needs __name__, __file__, __path__, __loader__ attribute
(define module-class
  (CClass
   'module
   (list 'object)
   (seq-ops (list
             (def '__init__
               (CFunc (list 'self 'name) (none)
                      (CAssign (CGetField (CId 'self (LocalId)) 'name)
                               (CId 'name (LocalId)))
                      true))))))
