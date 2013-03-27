#lang plai-typed/untyped
(require "python-phase1.rkt"
         "python-phase2.rkt")

(define (new-scope-phase expr)
  (phase2 (scope-phase expr)))

;; extract global bindings for module use 
;; This is not efficient BTW.
;; NOTE: Since the phase2 logic has been changing,
;; this function will be deprecated until phase2 becomes stable
;; -- Junsong
(define (get-module-level-globals expr)
  (extract-post-transform-globals
   (remove-nonlocal
    (collapse-pyseq
     (post-desugar
      (make-local-list
       empty
       (scope-phase expr)))))))

