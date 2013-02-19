#lang plai-typed

;; type - the metaclass of everything
(require "../python-core-syntax.rkt" 
         "../util.rkt")

(define type-class
  (seq-ops (list
             (CAssign (CId 'type (GlobalId))
                      (CClass 
                        'type
                        (list 'object)
                        (CNone)))
              ;; Only the 1 argument version is supported
              (def 'type '__init__
                (CFunc (list 'self 'obj) (none)
                       (CAssign (CId 'self (LocalId))
                                (CBuiltinPrim '$class (list (CId 'obj (LocalId)))))
                       (some 'type))))))
