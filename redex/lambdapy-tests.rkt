#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains reduction tests - terms and what they ;;
;; should reduce to (after any number of steps).            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require redex
         "lambdapy-reduction.rkt"
         "lambdapy-core.rkt"
         "lambdapy-prim.rkt")


(define (reduce-many e)
  (define t (apply-reduction-relation* λπ-red (term (,e (()) ()))))
  (when (not (equal? (length t) 1))
    (error (format "Ambiguous reduction: ~a" t)))
  (define results (first t))
  (if (not (= (length results) 3))
      (error (format "Resulted in non-term: ~a" (first t)))
      (first results)))

(define (expect e v)
  (define result (reduce-many e))
  (if (test-equal result v)
      (void)
      (error (format "Got: ~a, Should be: ~a" result v))))


;; Primitive values
(expect (term none) (term vnone))
(expect (term true) (term vtrue))
(expect (term false) (term vfalse))
(expect (term undefined) (term undefined-val))

;; control flow
(expect (term (if true none undefined)) (term vnone))
(expect (term (if false none undefined)) (term undefined-val))
(expect (term (if (exception-r vtrue) false false))
	(term (exception-r vtrue)))
(expect (term (seq true false)) (term vfalse))
(expect (term (seq (raise vtrue) false))
	(term (exception-r vtrue)))
(expect (term (seq false (raise vtrue)))
	(term (exception-r vtrue)))
(expect (term (seq break false))
	(term break-r))
(expect (term (seq (return true) false))
	(term (return-r vtrue)))
(expect (term (while true break false))
	(term break-r))


;; binding
(expect (term (let (x vtrue) (id x local)))
	(term vtrue))
(expect (term (let (x (exception-r vtrue)) false))
	(term (exception-r vtrue)))
(expect (term (let (x vtrue) (exception-r vfalse)))
	(term (exception-r vfalse)))

;; prims
(expect (term (prim2 "is" true true)) (term vtrue))
(expect (term (prim2 "is" true false)) (term vfalse))
(expect (term (prim2 "is" (num 1) (num 1))) (term vfalse))
