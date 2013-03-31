#lang racket

(require racket/pretty
	 "python-parser.rkt"
	 "parse-python.rkt"
	 "python-lexer.rkt"
	 "util.rkt")

(provide compare-parse)

; Take a port, provide its contents to native parser and python parser
; If equal?, return success message, otherwise a failure message wiht both ASTs

(define (compare-parse port)
  (let* ((program-text (port->string port))
	 (program-port-1 (open-input-string program-text))
	 (program-port-2 (open-input-string program-text))	 
     (python-ast 
      (with-handlers [(exn:fail? (lambda (e) 
                                   (display "Python parser error:\n" (current-error-port))
                                   (display e (current-error-port))
                                   (exit 1)))]
        
        (parse-python/port program-port-2 (get-pypath))))
	 (native-ast (with-handlers [(exn:fail? (lambda (e) e
                                                    (display "Native parser error:\n")
                                                    (pretty-print e)
                                                    (display "=== Python parser ===\n")
                                                    (pretty-write python-ast)
                                                    (exit 1)
                                                    ))]
		       (parse-python program-port-1))))
    (if (equal? native-ast python-ast)
        ;; parser currently displays unhandled portion
        (exit)
        (begin
          (display "Native parser did not match Python parser.\n") 
          (display "=== Native parser ===\n")
          (pretty-write native-ast)
          (display "=== Python parser ===\n")
          (pretty-write python-ast)
          (exit 1)))))
