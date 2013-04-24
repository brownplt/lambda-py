#lang racket

(require "../util.rkt"
         (prefix-in native: "python-parser.rkt")
         (prefix-in python: "python-python-parser.rkt"))

(provide parser native-parse-python/port native-parse-python-no-srcloc/port python-parse-python/port)

(define (native-parse-python/port port)
  (parameterize [(native:parser-src-loc #t)]
    (native:parse-python port)))

(define (native-parse-python-no-srcloc/port port)
  (parameterize [(native:parser-src-loc #f)]
    (native:parse-python port)))

(define python-parse-python/port (lambda (port) (python:parse-python/port port (get-pypath))))

(define parser (make-parameter native-parse-python/port))
