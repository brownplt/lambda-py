#lang plai-typed

;; type - the metaclass of everything
(require "../python-core-syntax.rkt" 
         "../util.rkt")

(define type-class
  (CClass 
    'type
    (list 'object)
    (seq-ops (list
              ;; This would be the 1 argument version of the constructor
              ;; when __new__ be available
              (def '__new__
                (CFunc (list 'obj) (none)
                       (CReturn
                        (CBuiltinPrim '$class (list (CId 'obj (LocalId)))))
                       (none)))))))
