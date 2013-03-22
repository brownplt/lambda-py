#lang racket/base

(require racket/pretty
         "parse-python.rkt"
         "get-structured-python.rkt"
         "python-interp.rkt"
         "python-phases.rkt"
         "python-desugar.rkt"
         "python-cps.rkt"
         "python-macros.rkt"
         "python-lib.rkt"
         "run-tests.rkt"
         "util.rkt"
         "python-evaluator.rkt")

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
            (parse-python/port port (get-pypath)))))))))

(define (get-surface-syntax port)
  (get-structured-python
   (parse-python/port port (get-pypath))))

(define (get-lexical-syntax port)
  (new-scope-phase
   (get-structured-python
    (parse-python/port port (get-pypath)))))

(define (desugar-w/lex port)
  (desugar
    (new-scope-phase
      (get-structured-python
        (parse-python/port port (get-pypath))))))

(define (desugar-w/lib port)
  (python-lib
   (desugar
    (new-scope-phase
     (get-structured-python
      (parse-python/port port (get-pypath)))))))

(define (desugar-w/macros port)
  (desugar
   (desugar-macros
    (new-scope-phase
     (get-structured-python
      (parse-python/port port (get-pypath)))))))


(define (get-core-syntax port)
  (desugar
    (new-scope-phase
      (get-structured-python
        (parse-python/port port (get-pypath))))))
