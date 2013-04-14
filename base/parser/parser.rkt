#lang racket

(require "../util.rkt"
         (prefix-in native: "python-parser.rkt")
         (prefix-in python: "python-python-parser.rkt"))

(provide parser native-parse-python/port python-parse-python/port)

(define native-parse-python/port native:parse-python)
(define python-parse-python/port (lambda (port) (python:parse-python/port port (get-pypath))))
(define parser (make-parameter native-parse-python/port))
