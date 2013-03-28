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
	 (native-ast (with-handlers [(exn:fail? (lambda (e) e))]
		       (parse-python program-port-1)))
	 (python-ast (parse-python/port program-port-2 (get-pypath))))
    (if (equal? native-ast python-ast)
        "Programs are equal?\n"
        (string-append "=== Native parser ===\n"
                       (pretty-format native-ast) 
                       "\n=== Python parser ===\n"
                       (pretty-format python-ast)
                       "\n=====================\n"))))
