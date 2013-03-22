#lang plai-typed/untyped
(require "python-phase1.rkt"
         "python-phase2.rkt")

(define (new-scope-phase expr)
  (phase2 (scope-phase expr)))

;; extract global bindings for module use 
;; This is not efficient BTW
;; -- Junsong
(define (get-module-level-globals expr)
  (extract-post-transform-globals
   (remove-nonlocal
    (remove-blocks
     (make-local-list
      empty
      (collapse-pyseq
       (post-desugar
        (scope-phase expr))))))))

