#lang plai-typed/untyped

(require "../python-core-syntax.rkt"
         "../util.rkt")

(define main-module-name '__main__)

(define main-module-id (CId main-module-name (GlobalId)))

(define main-module
  (seq-ops
   (list
    (CAssign main-module-id (CObject (CId '$module (GlobalId)) (none)))
    (set-field main-module-id '__name__
             (make-builtin-str (symbol->string main-module-name))))))
