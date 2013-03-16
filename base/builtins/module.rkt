#lang plai-typed/untyped

(require "../python-core-syntax.rkt"
         "type.rkt"
         "../util.rkt")

(define module-class : CExpr
  (seq-ops
   (list
    (CAssign (CId '$module (GlobalId))
             (builtin-class
              '$module
              (list 'object)
              (CNone)))
    (def '$module '__str__
         (CFunc (list 'self) (none)
                (CReturn (make-builtin-str "<module>"))
                (some '$module)))
    (def '$module '__name__
         (CNone)))))

