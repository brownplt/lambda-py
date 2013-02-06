#lang plai-typed

(require "../python-core-syntax.rkt"
         "../util.rkt")

(define module-class : CExpr
  (seq-ops
   (list
    (CAssign (CId '$module (GlobalId))
             (CClass
              '$module
              (list 'object)
              (CNone)))
    (def '$module '__str__
         (CFunc (list 'self) (none)
                (CReturn (CStr "<module>"))
                true)))))

;; the builtin primitive make-module,
;; which will take a dict, return a module object
(define (make-module args env sto) : (optionof CVal)
  (check-types args env sto '$dict
               (some
                (VObject '$module
                         (none)
                         (metadict->sym-addr-hash mval1)))))
