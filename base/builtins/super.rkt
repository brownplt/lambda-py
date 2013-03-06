#lang plai-typed/untyped

;; super - the super proxy class

(require "../python-core-syntax.rkt" 
         "../util.rkt"
         (typed-in racket/base (list-tail : ((listof 'a) number -> (listof 'a)))))

;; super-self: returns the active self, if any, from the stack
(define (super-self [stk : Stack]) : (optionof CVal)
  (local [(define (fetch-self [st : Stack]) : (optionof CVal)
            (cond
              [(empty? st) (none)]
              [(some? (Frame-self (first st))) (Frame-self (first st))]
              [else (fetch-self (rest st))]))]
    (if (> (length stk) 4) ;; first 4 frames are from super() instance creation
        (fetch-self (list-tail stk 4))
        (none))))

;; super-thisclass: returns the embodying class, if any, from the stack
(define (super-thisclass [stk : Stack]) : (optionof CVal)
  (local [(define (fetch-class [st : Stack]) : (optionof CVal)
            (cond
              [(empty? st) (none)]
              [(some? (Frame-class (first st))) (Frame-class (first st))]
              [else (fetch-class (rest st))]))]
    (if (> (length stk) 4) ;; first 4 frames are from super() instance creation
           (fetch-class (list-tail stk 4))
           (none))))
