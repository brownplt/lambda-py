#lang racket

(provide syntax->datum+stx-hash)

;; Transform a syntax-object (only S-Expressions handled right now) into an s-expression, as with syntax-datum, AND hasheq mapping each non-atom item to its original syntax object
(define (syntax->datum+stx-hash stx)
  (local ((define stx-hash (make-hasheq))
          (define (hash-datum! datum stx)
            (when (hash-has-key? stx-hash datum)
                (error (format "Datum ~a already mapped, but needs to be unique." datum)))
            (hash-set! stx-hash datum stx))
          (define (rec stx)
            ;; The syntax object produced by ragg is a touch odd in structure.
            (let* ((e (syntax-e stx))
                   (datum (cond 
                           [(number? e) e]
                           [(symbol? e) e]
                           
                           ;; Identifiers, AND (ugh) all string literals in grammar
                           [(string? e) 
                            (let ((datum (string-copy e)))
                              (begin (hash-datum! datum stx)
                                     datum))]
                           ;; productions
                           [(list? e) 
                            (let ((datum (map rec e)))
                              (begin (hash-datum! datum stx)
                                     datum))]
                           ;; Non-literal terminals
                           [(pair? e)
                            (let ((datum(cons (rec (car e))
                                              (rec (cdr e)))))
                              (begin (hash-datum! datum stx)
                                     datum))]
                           [else (error (format "Unhandled e case: ~a" e))])))
              datum)))
    (values (rec stx) stx-hash)))
          
