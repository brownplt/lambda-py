#lang racket

(require redex
         (only-in "../base/python-lib-bindings.rkt" lib-function-dummies lib-functions bind-left bind-right)
         "../base/python-lib.rkt"
         (only-in "../base/util.rkt" seq-ops)
         "../base/python-core-syntax.rkt"
         "../base/python-tools.rkt"
         "core-to-redex.rkt"
         "lambdapy-reduction.rkt"
         "lambdapy-core.rkt"
         "lambdapy-prim.rkt"
         (only-in plai-typed/untyped some none))
(provide (all-defined-out)
         some none
         (all-from-out
          redex
          "lambdapy-prim.rkt"
          "lambdapy-core.rkt"
          "lambdapy-reduction.rkt"
          "../base/python-core-syntax.rkt"
          "../base/python-lib-bindings.rkt"
          "../base/python-tools.rkt"
          "../base/python-lib.rkt"
          "core-to-redex.rkt"))

(define lp-traces (lambda (t) (traces λπ-red t)))

(define (core-str str)
  (CObject (CId '%str (GlobalId)) (some (MetaStr str))))

(define (redex-str str)
  (term (alloc (obj-val %str (meta-str ,str) ()))))

(define (redex-strv str)
  (term (obj-val %str (meta-str ,str) ())))

(define (python->redex str)
  (core->redex (get-core-syntax (open-input-string str))))

(set-pypath "/home/joe/src/Python-3.2.3/python")

(define-syntax (expect stx)
  (syntax-case stx ()
      ((_ e v)
       #'(test-->>∃ #:steps 1000000
                    λπ-red (term (e () ()))
                    (λ (p) (equal? (term v) (first p)))))))

(define-syntax (expect-raw stx)
  (syntax-case stx ()
      ((_ e pat)
       #'(test-->>∃ λπ-red (term (,e () ()))
                    (λ (p) (redex-match? λπ pat (first p)))))))


(define last-term (box #f))
(define success (box #f))
(define (get-last-term)
  (unbox last-term))
(define-syntax (full-expect stx)
  (syntax-case stx ()
      ((_ (e ε Σ) pat)
       #'(begin
           (set-box! last-term #f)
           (set-box! success #f)
           (test-->>∃ #:steps 1000000
                      λπ-red (term (e ε Σ))
                      (λ (p)
                        (if (not
                             (and
                              (redex-match? λπ pat p)))
                            (begin (set-box! last-term p) #f)
                            (begin (set-box! success #t) #t))))
           (when (not (unbox success))
             (display "Last term:\n") (pretty-write (unbox last-term)))))))

(define py-none (CId 'None (GlobalId)))

(define inherit-ε (term {(%str 5) (None 13)}))
(define inherit-Σ (term {(4 (obj-val type (meta-class %str) (("__mro__" 9) ("not-inherited" 6))))
   (5 (pointer-val 4))
   (6 (pointer-val 7))
   (7 (obj-val %str (meta-str "should not be looked up") ()))
   (8 (obj-val %object (no-meta) (("inherited" 11))))
   (9 (pointer-val 10))
   (10 (obj-val %tuple (meta-tuple ((pointer-val 4) (pointer-val 8))) ()))
   (11 (pointer-val 12))
   (12 (obj-val %function (meta-closure (λ (self) (no-var) vnone)) ()))
   (13 (obj-val %none (meta-none) ()))}))