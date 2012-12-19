#lang racket

(require redex)
(require "lambdapy-core.rkt")

(define red
  (reduction-relation
   λπ
   #:domain p
   (--> (in-hole P (str string))
        (in-hole P (obj-val str (meta-str string) ()))
        "string")
   (--> (in-hole P none)
        (in-hole P vnone)
        "none")
   (--> (in-hole P undefined)
        (in-hole P (undefined-val))
        "undefined")
   (--> (in-hole P (if val e_1 e_2))
        (in-hole P e_1)
        (side-condition (term (truthy? val)))
        "if-true")
   (--> (in-hole P (if val e_1 e_2))
        (in-hole P e_2)
        (side-condition (not (term (truthy? val))))
        "if-false")
   (--> (in-hole P (seq val e))
        (in-hole P e)
        "seq")
   (--> ((in-hole E (assign (id x_1 local) val_1))
         ((name scope ((x_2 ref) ...)) ε ...)
         (name store ((ref_2 val) ...)))
        ((in-hole E vnone)
         ((extend-env scope x_1 ref_1) ε ...)
         (override-store store ref_1 val_1))
        (side-condition (not (member (term x_1) (term (x_2 ...)))))
        (side-condition (not (member (term ref_1) (term (ref_2 ...)))))
        (where ref_1 ,(new-loc))
        "assign-local-free")
   (--> ((in-hole E (assign (id x_1 local) val_1))
         (name env (((x_2 ref) ... (x_1 ref_1) (x_3 ref) ...) ε ...))
         (name store ((ref_2 val) ... (ref_1 val_0) (ref_3 val) ...)))
        ((in-hole E vnone)
         env
         (override-store store ref_1 val_1))
        (side-condition (not (member (term x_1) (term (x_2 ... x_3 ...)))))
        (side-condition (not (member (term ref_1) (term (ref_2 ... ref_3 ...)))))
        "assign-local-bound")
   ))

(define-metafunction λπ
  truthy? : val -> #t or #f
  [(truthy? (fun-val any ...)) #t]
  [(truthy? (obj-val any ...)) #t] ;; simply return #t for now
  [(truthy? (undefined-val)) #f])

(define-metafunction λπ
  extend-env : ε x ref -> ε
  [(extend-env ((x_1 ref_1) ...) x ref) ((x_1 ref_1) ... (x ref))])

(define-metafunction λπ
  override-store : Σ ref val -> Σ
  [(override-store ((ref_2 val_2) ...) ref_1 val_1)
   ((ref_2 val_2) ... (ref_1 val_1))
   (side-condition (not (member (term ref_1) (term (ref_2 ...)))))]
  [(override-store ((ref_2 val_2) ... (ref_1 val_0) (ref_3 val_3) ...) ref_1 val_1)
   ((ref_2 val_2) ... (ref_1 val_1) (ref_3 val_3) ...)
   (side-condition (not (member (term ref_1) (term (ref_2 ...)))))])

(define-term vnone (obj-val none (meta-none) ()))

(define new-loc
  (let ([n 0])
    (lambda () (begin (set! n (add1 n)) n))))
