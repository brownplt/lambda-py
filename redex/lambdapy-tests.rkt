#lang racket

;; This file imports sub-test files.  The imports can be easily commented out
;; to only run subsets of the tests.

;; TODO(joe): I have intermittently gotten module-path-index-resolve: "self" index has no resolution
;; module path index: #<module-path-index> when running this, but individual
;; test files run fine

(require
 redex
 "lambdapy-basics.rkt"
 "lambdapy-num-tests.rkt" 
 "lambdapy-func-tests.rkt"
 "lambdapy-core-tests.rkt"
 "lambdapy-obj-tests.rkt"
 "lambdapy-context-test.rkt"
 "lambdapy-test-skull.rkt"
 "lambdapy-module-tests.rkt"
 )

(test-results)
