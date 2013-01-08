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


def __import__(name):
   m = module(name)
   f = open(name+'.py', 'r')
   code = f.read()
   f.close()
   m.__dict__ = exec_to_dict(code)
   return m



") 
