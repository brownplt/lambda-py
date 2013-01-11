#lang plai-typed

(require "../python-core-syntax.rkt"
         "../util.rkt")

(define module-class : CExpr
  (CClass
   '$module
   (list 'object)
   (seq-ops (list 
              (def '__str__
                   (CFunc (list 'self) (none)
                          (CReturn (CStr "<module>"))
                          true))
              ))))

(define (make-module args env sto) : (optionof CVal)
  (check-types args env sto '$simpledict
               (some 
                (VObject '$module 
                         (none) 
                         (MetaSimpleDict-contents mval1)))))