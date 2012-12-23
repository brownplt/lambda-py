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
  (define t (apply-reduction-relation* λπ-red (term (,e () ()))))
  (when (not (equal? (length t) 1))
    (error (format "Ambiguous reduction: ~a" t)))
  (define results (first t))
  (if (not (= (length results) 3))
      (error (format "Resulted in non-term: ~a" (first t)))
      (first results)))

(define (expect-val e v)
  (define result (reduce-many e))
  (if (test-equal result v)
      (void)
      (error (format "Got: ~a, Should be: ~a" result v))))


;; Primitive values
(expect-val (term none) (term vnone))
(expect-val (term true) (term vtrue))
(expect-val (term false) (term vfalse))
(expect-val (term undefined) (term undefined-val))

;; control flow
(expect-val (term (if true none undefined)) (term vnone))
(expect-val (term (if false none undefined)) (term undefined-val))
