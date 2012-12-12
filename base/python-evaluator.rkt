#lang racket

(require racket/runtime-path)

(provide mk-proc-eval/silent mk-python-cmdline-eval)

;; mk-proc-eval/inner : (string * input-port -> 'a) ('a -> void) (port * error -> void)
;; -> (pairof string string)

;; run the evaluator with the given name, catching errors and handing
;; output off to the value-printer (in case of success) and
;; error-printer (in case of failure)
;; Capture all the output in two strings, which are returned as a pair.
(define (mk-proc-eval/inner evaluator value-printer error-printer)
  (λ (name port)
    (define out (open-output-string "eval-out"))
    (define err (open-output-string "eval-err"))
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (with-handlers
        ([exn:fail?
         (λ (exn) (error-printer (exn-message exn) (current-error-port)))])
        (define result (evaluator name port))
        (value-printer result))
      (define result (cons (get-output-string out) (get-output-string err)))
      (close-output-port out)
      (close-output-port err)
      result)))

;; mk-proc-eval/silent : (string * input-port -> 'a) -> (pairof string string)
;; Here, all output and errors must come from running the evaluator; the
;; harness is set to do no printing
(define (mk-proc-eval/silent evaluator)
  (mk-proc-eval/inner evaluator
    (λ (value) '())
    (λ (error port) (display error port))))

(define PRELUDE "py-prelude.py")

(define (mk-python-cmdline-eval some-cmdline-path)
  (λ (name port)
    (define input-src (port->string port))
    (define proc (process some-cmdline-path))
    (display (file->string PRELUDE) (second proc))
    (display input-src (second proc))
    (display eof (second proc))
    (flush-output (second proc))
    (close-output-port (second proc))
    ((fifth proc) 'wait)
    (define stdout (port->string (first proc)))
    (define stderr (port->string (fourth proc)))
    (close-input-port (fourth proc))
    (close-input-port (first proc))
    (cons stdout stderr)))

