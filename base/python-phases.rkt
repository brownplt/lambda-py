#lang plai-typed/untyped
(require "python-phase1.rkt"
         "python-phase2.rkt")

(define (new-scope-phase expr)
  (phase2 (scope-phase expr)))
