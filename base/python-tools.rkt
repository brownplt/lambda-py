#lang racket/base

(require racket/pretty
         "get-structured-python.rkt"
         "python-interp.rkt"
         "python-phases.rkt"
         "python-phase2.rkt"
         "python-phase1.rkt"
         "python-desugar.rkt"
         "python-cps.rkt"
         "python-macros.rkt"
         "python-lib.rkt"
         "run-tests.rkt"
         "util.rkt"
         "python-evaluator.rkt"
         "parser/parser.rkt"
         "parser/python-lexer.rkt"
         "parser/test-parser.rkt")

(provide (all-defined-out)
         set-pypath
         get-pypath)

(define (python-test-runner _ port)
  (run-python port))

(define (run-python port)
  (interp
   (python-lib
    (desugar-generators
     (desugar
      (new-scope-phase
       (get-structured-python
        ((parser) port))))))))

(define (get-surface-syntax port)
  (get-structured-python
   ((parser) port)))

(define (get-lexical-syntax port)
  (phase2-without-locals (scope-phase
                          (get-structured-python
                           ((parser) port)))))

(define (get-phase1-syntax port)
  (scope-phase
   (get-structured-python
    ((parser) port))))

(define (get-lexical-syntax-with-locals port)
  (new-scope-phase
   (get-structured-python
    ((parser) port))))

(define (desugar-w/lex port)
  (desugar
   (new-scope-phase
    (get-structured-python
     ((parser) port)))))

(define (desugar-w/lib port)
  (python-lib
   (desugar
    (new-scope-phase
     (get-structured-python
      ((parser) port))))))

(define (desugar-w/macros port)
  (desugar
   (new-scope-phase
    (get-structured-python
     ((parser) port)))))

(define (get-core-syntax port)
  (desugar
   (new-scope-phase
    (get-structured-python
     ((parser) port)))))

(define (get-lexer-tokens port)
  (lex-all port))
