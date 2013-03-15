#lang racket/base

(require (planet dherman/json:4:0))
(require racket/cmdline
         racket/pretty
         "parse-python.rkt"
         "get-structured-python.rkt"
         "python-interp.rkt"
         "python-phases.rkt"
         "python-desugar.rkt"
         "python-macros.rkt"
         "python-lib.rkt"
         "run-tests.rkt"
         "util.rkt"
         "python-evaluator.rkt")

(define (python-test-runner _ port)
  (run-python port))

(define (run-python port)
  (interp
    (python-lib
      (desugar
        (new-scope-phase
          (get-structured-python
            (parse-python/port port (get-pypath))))))))

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

(command-line
  #:once-each
  ("--interp" "Interpret stdin as python"
   (run-python (current-input-port)))

  ("--interp-py" "Interpret stdin as python using py-prelude.py"
   (define results ((mk-python-cmdline-eval (get-pypath)) "stdin" (current-input-port)))
   (display (car results) (current-output-port))
   (display (cdr results) (current-error-port)))

  ("--get-syntax" "Get s-exp for python"
   (pretty-write (parse-python/port (current-input-port) (get-pypath))))
  
  ("--get-surface-syntax" "Get surface syntax python"
   (pretty-write (get-surface-syntax (current-input-port))))

  ("--get-lexical-syntax" "Get surface syntax python"
   (pretty-write (get-lexical-syntax (current-input-port))))

  ("--get-core-syntax" "Get desugared python"
   (pretty-write (get-core-syntax (current-input-port))))

  ("--get-core-syntax-with-libs" "Get desugared python and libraries (big)"
   (pretty-write (desugar-w/lib (current-input-port))))

  ("--get-core-syntax-with-macros" "Get desugared python with ___ macros expanded"
   (pretty-write (desugar-w/macros (current-input-port))))

  
  ("--test" dirname "Run all tests in dirname"
   (display (results-summary (run-tests (mk-proc-eval/silent python-test-runner) dirname))))

  ("--test-py" dirname "Run all tests in dirname using python"
   (display (results-summary (run-tests (mk-python-cmdline-eval (get-pypath)) dirname))))

  ("--test-cps" "Run cps tests"
   ;; NOTE(dbp): this is somewhat of a hack, but cps-test.rkt has a
   ;; bunch of (test ...) statements, and this will cause them to be run.
   (dynamic-require "cps-test.rkt" 0))

  ("--python-path" path "Set the python path" 
   (set-pypath path))

  ("--progress-report" dirname "Generate a soft report"
   (printf "~a\n"
    (jsexpr->json
     (json-summary
      (run-tests (mk-proc-eval/silent python-test-runner) dirname)))))
)

