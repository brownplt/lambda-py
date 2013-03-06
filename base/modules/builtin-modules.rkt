#lang plai-typed/untyped

(require "sys.rkt")


(define (get-builtin-module-names)
  (map (lambda (x)
         (first x))
       (builtin-modules-list)))

(define (get-builtin-modules)
  (map (lambda (x)
         (second x))
       (builtin-modules-list)))

;; add new built-in modules info here, then all should work fine in
;; python-lib and python-lib-bindings. 
(define (builtin-modules-list)
  (list
   (list sys-module-name sys-module)
   ))


