#lang plai-typed

;; type - the metaclass of everything
(require "../python-core-syntax.rkt" 
         "../util.rkt")

(define type-class
  (CClass 
    'type
    (list 'object)
    (seq-ops (list
              ;; Only the 1 argument version is supported
              (def '__init__
                (CFunc (list 'self 'obj) (none)
                       (CAssign (CId 'self (LocalId))
                                (CBuiltinPrim '$class (list (CId 'obj (LocalId)))))
                       (none)))))))
