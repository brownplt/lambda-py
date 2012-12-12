#lang racket/base

(require racket/system racket/list)
(require (planet dherman/json:4:0))

(define (get-parsed-json input-port python-path)
  (define stdout (open-output-string "stdout"))
  (define stderr (open-output-string "stderr"))
  (define proc (process*/ports stdout input-port stderr python-path "python-parser.py"))
  ((fifth proc) 'wait) 
  (define err-output (get-output-string stderr))
  (when (not (equal? err-output ""))
    (error 'parse (format "Couldn't parse python file with python-parser.py.  Error was: \n ~a" err-output)))
  (define std-output (get-output-string stdout))
  (json->jsexpr std-output))

(define (parse-python/string s python-path)
  (parse-python/port (open-input-string s) python-path))

(define (parse-python/port port python-path)
  (get-parsed-json port python-path))

(provide parse-python/port parse-python/string)

