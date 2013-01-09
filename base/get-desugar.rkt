#lang racket

(require "get-structured-python.rkt"
         "python-desugar.rkt"
         "parse-python.rkt")

;;; for testing
(define (get-desugar s)
  (pretty-print
   (desugar
    (get-structured-python
     (parse-python/string s "/usr/bin/python3")))))

(get-desugar "

{'2':3, 'x':5}


") 
