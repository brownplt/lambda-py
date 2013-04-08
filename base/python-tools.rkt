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
         "parser/python-python-parser.rkt"
         "parser/python-lexer.rkt"
         "parser/python-parser.rkt"
         "parser/python-grammar.rkt"
         "parser/test-parser.rkt")

(provide (all-defined-out)
         set-pypath
         get-pypath)

(define (python-test-runner _ port)
  (run-python port))

(define (python-test-runner-nopy _ port)
  (run-python-nopy port))

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
  (phase2-without-locals (scope-phase
   (get-structured-python
    (parse-python/port port (get-pypath))))))

(define (get-phase1-syntax port)
  (scope-phase
   (get-structured-python
    (parse-python/port port (get-pypath)))))

(define (get-lexical-syntax-with-locals port)
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

(define (get-lexer-tokens port)
  (lex-all port))

(define (get-parse-tree port)
  (parse-python port))

(define (parse-test port)
  (single-parse-test port))

(define (run-python-nopy port)
  (interp
    (python-lib
      (desugar
        (new-scope-phase
	 (get-structured-python
	   (parse-python port)))))))

