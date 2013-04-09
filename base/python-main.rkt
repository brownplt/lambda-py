#lang racket/base

(require (planet dherman/json:4:0))
(require racket/cmdline
         racket/pretty
         "python-tools.rkt"
         "run-tests.rkt"
         "util.rkt"
         "python-evaluator.rkt"
         "python-lexical-printer.rkt"
         "python-evaluator.rkt"
         "parser/parser.rkt"
         "parser/test-parser.rkt")

(command-line
  #:once-each
  ("--interp" "Interpret stdin as python"
   (run-python (current-input-port)))

  ("--interp-py" "Interpret stdin as python using py-prelude.py"
   (define results ((mk-python-cmdline-eval (get-pypath)) "stdin" (current-input-port)))
   (display (car results) (current-output-port))
   (display (cdr results) (current-error-port)))

  ("--get-syntax" "Get s-exp for python"
   (pretty-write ((parser) (current-input-port))))
  
  ("--get-surface-syntax" "Get surface syntax python"
   (pretty-write (get-surface-syntax (current-input-port))))

  ("--get-lexical-syntax" "Get surface syntax python"
   (lexexpr-print (get-lexical-syntax (current-input-port)) ))

  ("--get-phase1-syntax" "Get surface syntax python"
   (lexexpr-print (get-phase1-syntax (current-input-port)) ))

  ("--get-lexical-syntax-old" ""
   (pretty-write (get-lexical-syntax (current-input-port))))

    ("--get-lexical-syntax-with-locals" "Get surface syntax python"
   (lexexpr-print (get-lexical-syntax-with-locals (current-input-port))))

  ("--get-core-syntax" "Get desugared python"
   (pretty-write (get-core-syntax (current-input-port))))

  ("--get-core-syntax-with-libs" "Get desugared python and libraries (big)"
   (pretty-write (desugar-w/lib (current-input-port))))

  ("--get-core-syntax-with-macros" "Get desugared python with ___ macros expanded"
   (pretty-write (desugar-w/macros (current-input-port))))

  ("--get-lex-tokens" "Get tokens from native lexer"
   (pretty-write (get-lexer-tokens (current-input-port))))

  ("--test" dirname "Run all tests in dirname"
   (display (results-summary (run-tests (mk-proc-eval/silent python-test-runner) dirname))))

  ("--test-py" dirname "Run all tests in dirname using python"
   (display (results-summary (run-tests (mk-python-cmdline-eval (get-pypath)) dirname))))

  ("--test-parser" "Compare native parser results with Python parser for input"
   (parse-test (current-input-port)))

  ("--test-native-parser" dirname "Run all tests in dirname through native/python parser test"
   (parse-tests dirname))

  ("--test-cps" "Run cps tests"
   ;; NOTE(dbp): this is somewhat of a hack, but cps-test.rkt has a
   ;; bunch of (test ...) statements, and this will cause them to be run.
   (dynamic-require "cps-test.rkt" 0))

  ("--python-path" path "Set the python path" 
   (set-pypath path))

  ("--python-parser" "Use the python selected with --python-path to parse python"
   (parser python-parse-python/port))

  ("--native-parser" "Use the native racket parser to parse python (default)"
   (parser native-parse-python/port))

  ("--progress-report" dirname "Generate a soft report"
   (printf "~a\n"
    (jsexpr->json
     (json-summary
      (run-tests (mk-proc-eval/silent python-test-runner) dirname)))))
)

