#lang racket

(require redex
         "lambdapy-reduction.rkt"
         "lambdapy-core.rkt")

(define match-result (term-match λπ [(e εs Σ) (term (e εs Σ))]))
  
(define (valid? e)
  (define t (apply-reduction-relation* λπ-red (term (,e () ()))))
  (when (not (equal? (length t) 1))
    (error (format "Ambiguous reduction: ~a" t)))
  (define results (match-result (first t)))
  (void (when (not (= (length results) 1))
          (error (format "Resulted in non-term: ~a" (first t))))))

(valid? (term none))

(valid? (term true))