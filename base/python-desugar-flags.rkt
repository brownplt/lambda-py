#lang plai-typed/untyped


;;; desugar flags
;; flags for assignment
(define dsg-subscript-assignment true)
(define dsg-tuple-assignment true)
;; flags for func
(define dsg-func-kwonlyargs true)
(define dsg-func-kwarg true)
;; flags for built-in primes
(define dsg-built-in-prims true)
;; flags for for statement
(define dsg-for true)
;; flags for exception, try, with statement
(define dsg-with true)
(define dsg-try-finally true)
(define dsg-try-except-else true)
;; flags for function exec
(define dsg-app true)
;; flags for subscript
(define dsg-subscript true)
