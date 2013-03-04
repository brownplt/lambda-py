#lang plai-typed/untyped

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
                (some '$module))))))

;; the builtin primitive make-module,
;; which will take a dict, return a module object

;; make-module cannot use dict as argument to return
;; module object any more
;; (define (make-module args env sto) : (optionof CVal)
;;   (check-types args env sto 'dict
;;                (some
;;                 (VObject '$module
;;                          (none)
;;                          (metadict->sym-addr-hash mval1)))))
