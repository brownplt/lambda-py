#lang plai-typed/untyped

(require "sys.rkt"
         "../python-core-syntax.rkt")

(define (get-builtin-module-names)
  (map (lambda (x)
         (Module-name x))
       (builtin-modules-list)))

(define (get-builtin-modules)
  (map (lambda (x)
         (Module-object x))
       (builtin-modules-list)))

;; add new built-in modules info here, then all should work fine in
;; python-lib and python-lib-bindings. 
(define (builtin-modules-list)
  (list
   (Module sys-module-name sys-module)
   ))


