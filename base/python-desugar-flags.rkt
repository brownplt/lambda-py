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
;; flags for function exec. Check the util.rkt
;; TODO: try to desugar LexApp to LexPass(an empty lambda)
(define dsg-app true)
;; flags for subscript
(define dsg-subscript true)
;; flags for get object fields
(define dsg-dot-field true)

;; flags for while statement
(define dsg-while true)
;; flags for delete
(define dsg-delete true)
;; flags for augmented assignment
(define dsg-augassignment true)
;; flags for set attribute
(define dsg-expr-assign true)
;; flags for get attribute
(define dsg-expr-field true)
;; flags for class: TODO: need more specific?
(define dsg-class true)
;; flags for tuple
(define dsg-tuple true)
;; flags for list
(define dsg-list true)
;; flags for set
(define dsg-set true)
;; flags for dict
(define dsg-dict true)
;; flags for lambda
(define dsg-lam true)
;; flags for assert
(define dsg-assert true)
;; flags for pass: this is interesting--how important the pass statement in Python?
;; no idea how to write the code
;; (define dsg-pass true)
;; flags for raise
(define dsg-raise true)
;; flags for localLet and global let
(define dsg-locallet true)
(define dsg-globallet true)


;; may be useful
(define dsg-listcomp true)
(define dsg-compop true)
(define dsg-boolop true)
(define dsg-unaryop true)
(define dsg-binop true)



; No desugaring: LexReturn, LexBreak, LexContinue, 

;; TODO: decorator
