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


(define the-stdin (current-input-port))
(define the-stdout (current-output-port))
(define the-stderr (current-error-port))

(define (do-with-ports f)
  (parameterize
    ([current-input-port the-stdin]
     [current-output-port the-stdout]
     [current-error-port the-stderr])
  (f)))

(command-line
  #:once-each
  ("--stdin" filename "Provide a file to use as stdin"
    (set! the-stdin (open-input-file filename)))
  ("--stdout" filename "Provide a file to use as stdout"
    (set! the-stdout (open-output-file filename)))
  ("--stderr" filename "Provide a file to use as stderr"
    (set! the-stderr (open-output-file filename)))

  ("--interp" "Interpret stdin as python"
   (do-with-ports (λ () (run-python (current-input-port)))))

  ("--interp-py" "Interpret stdin as python using py-prelude.py"
   (do-with-ports
    (λ ()
       (define results ((mk-python-cmdline-eval (get-pypath)) "stdin" (current-input-port)))
       (display (car results) (current-output-port))
       (display (cdr results) (current-error-port)))))

  ("--get-syntax" "Get s-exp for python"
   (do-with-ports
    (λ ()
       (pretty-write ((parser) (current-input-port))))))

  ("--get-surface-syntax" "Get surface syntax python"
   (do-with-ports
    (λ ()
       (pretty-write (get-surface-syntax (current-input-port))))))

  ("--get-lexical-syntax" "Get surface syntax python"
   (do-with-ports
    (λ ()
       (lexexpr-print (get-lexical-syntax (current-input-port)) ))))

  ("--get-phase1-syntax" "Get surface syntax python"
   (do-with-ports
    (λ ()
       (lexexpr-print (get-phase1-syntax (current-input-port)) ))))

  ("--get-lexical-syntax-old" ""
   (do-with-ports
    (λ ()
       (pretty-write (get-lexical-syntax (current-input-port))))))

   ("--get-lexical-syntax-with-locals" "Get surface syntax python"
   (do-with-ports
    (λ ()
       (lexexpr-print (get-lexical-syntax-with-locals (current-input-port))))))

  ("--get-core-syntax" "Get desugared python"
   (do-with-ports
    (λ ()
       (pretty-write (get-core-syntax (current-input-port))))))

  ("--get-core-syntax-with-libs" "Get desugared python and libraries (big)"
   (do-with-ports
    (λ ()
       (pretty-write (desugar-w/lib (current-input-port))))))

  ("--get-core-syntax-with-macros" "Get desugared python with ___ macros expanded"
   (do-with-ports
    (λ ()
       (pretty-write (desugar-w/macros (current-input-port))))))

  ("--get-lex-tokens" "Get tokens from native lexer"
   (do-with-ports
    (λ ()
       (pretty-write (get-lexer-tokens (current-input-port))))))

  ("--test" dirname "Run all tests in dirname"
   (do-with-ports
    (λ ()
       (display (results-summary (run-tests (mk-proc-eval/silent python-test-runner) dirname))))))

  ("--test-py" dirname "Run all tests in dirname using python"
   (do-with-ports
    (λ ()
       (display (results-summary (run-tests (mk-python-cmdline-eval (get-pypath)) dirname))))))

  ("--test-native-parser" dirname "Run all tests in dirname through native/python parser test"
   (do-with-ports
    (λ ()
       (parse-tests dirname))))

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

  ;; For diff testing against --python-parser
  ("--native-parser-no-srcloc" "Use the native racket parser to parse python, excluding source position information"
   (parser native-parse-python-no-srcloc/port))
)
