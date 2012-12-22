#lang racket

(require redex)
(require redex/tut-subst)
(require "lambdapy-core.rkt"
         "lambdapy-prim.rkt")

(provide λπ-red)


(define λπ-red
  (reduction-relation
   λπ
   #:domain p
   (==> (str string)
        (obj-val str (meta-str string) ())
        "string")
   (==> none
        vnone
        "none")
   (==> undefined
        undefined-val
        "undefined")
   (==> (list (val ...))
        (obj-val list (meta-list (val ...)) ())
        "list")
   (==> (list (r))
        r
        (side-condition (not (val? (term r))))
        "list-nonval")
   (==> (tuple (val ...))
        (obj-val tuple (meta-tuple (val ...)) ())
        "tuple")
   (==> (tuple (r))
        r
        (side-condition (not (val? (term r))))
        "tuple-nonval")
   (--> ((in-hole E (fun (x ...) e)) εs Σ)
        ((in-hole E (fun-val εs (λ (x ...) e))) εs Σ)
        "fun-novararg")
   (--> ((in-hole E (fun (x ...) x_1 e)) εs Σ)
        ((in-hole E (fun-val εs (λ (x ...) (x_1) e))) εs Σ)
        "fun-vararg")
   (==> (object x)
        (obj-val x ())
        "object")
   (==> (object x mval)
        (obj-val x mval ())
        "object-mval")
   (--> ((in-hole E (prim1 op val)) εs Σ)
        ((in-hole E (δ op val εs Σ)) εs Σ)
        "prim1")
   (==> (prim1 op r)
        r
        (side-condition (not (val? (term r))))
        "prim1-nonval") ;; TODO: raise error for break, return
   (--> ((in-hole E (prim2 op val_1 val_2)) εs Σ)
        ((in-hole E (δ op val_1 val_2 εs Σ)) εs Σ)
        "prim2")
   (==> (prim2 op r e)
        r
        (side-condition (not (val? (term r))))
        "prim2-nonval-l") ;; TODO: raise error for break, return
   (==> (prim2 op val r)
        r
        (side-condition (not (val? (term r))))
        "prim2-nonval-r") ;; TODO: raise error for break, return
   (==> ((in-hole E (builtin-prim op (val ...))) εs Σ)
        ((in-hole E (δ op val ... εs Σ)) εs Σ)
        "builtin-prim")
   (==> (builtin-prim op (r))
        r
        (side-condition (not (val? (term r))))
        "builtin-prim-nonval") ;; TODO: raise error for break, return
   (==> (if val e_1 e_2)
        e_1
        (side-condition (term (truthy? val)))
        "if-true")
   (==> (if val e_1 e_2)
        e_2
        (side-condition (not (term (truthy? val))))
        "if-false")
   (==> (if (exception-r val) e_1 e_2)
        (exception-r val)
        "if-exc") ;; TODO: break-r and return-r throw SyntaxError
   (==> (seq val e)
        e
        "seq")
   (==> (seq r e)
        r
        (side-condition (not (val? (term r))))
        "seq-nonval")
   (==> (while e_1 e_2 e_3)
        (if e_1 (seq e_2 (while e_1 e_2 e_3)) e_3) ;; not handle break yet
        "while")
   (==> break
        break-r
        "break")
   #|
   (--> ((in-hole E (let (x_1 val) e))
         (ε_1 ε ...)
         Σ)
        ((in-hole E (subst (x_1) (x_new) e))
         ((extend-env ε_1 x_new ref_new) ε ...)
         (override-store Σ ref_new val))
        (fresh x_new)
        (where ref_new ,(new-loc))
        "let")|#
   (--> ((in-hole E (let (x_1 val_1) e))
         (ε_1 ε ...)
         Σ)
        ((in-hole E e)
         ((extend-env ε_1 x_1 ref_1) ε ...) ;; let-body's env will leak out, but it seems interp does it
         (override-store Σ ref_1 val_1))
        (where ref_1 ,(new-loc))
        "let")
   (--> ((in-hole E (let (x_1 (return-r val_1)) e))
         (ε_1 ε ...)
         Σ)
        ((in-hole E e)
         ((extend-env ε_1 x_1 ref_1) ε ...)
         (override-store Σ ref_1 val_1))
        (where ref_1 ,(new-loc))
        "let-ret")
   (--> ((in-hole E (let (x_1 (break-r)) e))
         (ε_1 ε ...)
         Σ)
        ((in-hole E e)
         ((extend-env ε_1 x_1 ref_1) ε ...)
         (override-store Σ ref_1 vnone))
        (where ref_1 ,(new-loc))
        "let-brk")
   (--> ((in-hole E (let (x_1 (exception-r val_1)) e))
         (ε_1 ε ...)
         Σ)
        ((in-hole E e)
         ((extend-env ε_1 x_1 ref_1) ε ...)
         (override-store Σ ref_1 val_1))
        (where ref_1 ,(new-loc))
        "let-exc")
   (--> ((in-hole E (id x_1 local))
         (name env (((x_2 ref_2) ... (x_1 ref_1) (x_3 ref_3) ...) ε ...))
         (name store ((ref_4 val_4) ... (ref_1 val_1) (ref_5 val_5) ...)))
        ((in-hole E val_1)
         env
         store)
        (side-condition (not (member (term x_1) (term (x_2 ... x_3 ...)))))
        (side-condition (not (member (term ref_1) (term (ref_4 ... ref_5 ...)))))
        (side-condition (not (redex-match? λπ (undefined-val) (term val_1)))) ;; TODO: exception for undefined
        "id-local")
   (--> ((in-hole E (get-field (obj-val x mval ... ((string_2 ref_2) ... (string_1 ref_1) (string_3 ref_3) ...)) string_1))
         εs
         (name store ((ref_4 val_4) ... (ref_1 val_1) (ref_5 val_5) ...)))
        ((in-hole E val_1)
         εs
         store)
        (side-condition (not (member (term string_1) (term (string_2 ... string_3 ...)))))
        (side-condition (not (member (term ref_1) (term (ref_4 ... ref_5 ...)))))
        "get-field")
   (--> ((in-hole E (get-field (obj-val x mval ... ((string_2 ref_2) ... ("__class__" ref_1) (string_3 ref_3) ...)) string_1))
         εs
         (name store ((ref_4 val_4) ... (ref_1 val_1) (ref_5 val_5) ...)))
        ((in-hole E (get-field val_1 string_1))
         εs
         store)
        (side-condition (not (string=? "__class__" (term string_1))))
        (side-condition (not (member (term string_1) (term (string_2 ... string_3 ...)))))
        (side-condition (not (member "__class__" (term (string_2 ... string_3 ...)))))
        (side-condition (not (member (term ref_1) (term (ref_4 ... ref_5 ...)))))
        "get-field-class")
   (--> ((in-hole E (assign (id x_1 local) val_1))
         ((name scope ((x_2 ref_2) ...)) ε ...)
         Σ)
        ((in-hole E vnone)
         ((extend-env scope x_1 ref_1) ε ...)
         (override-store Σ ref_1 val_1))
        (side-condition (not (member (term x_1) (term (x_2 ...)))))
        (where ref_1 ,(new-loc))
        "assign-local-free")
   (--> ((in-hole E (assign (id x_1 local) val_1))
         (name env (((x_2 ref_2) ... (x_1 ref_1) (x_3 ref_3) ...) ε ...))
         Σ)
        ((in-hole E vnone)
         env
         (override-store Σ ref_1 val_1))
        (side-condition (not (member (term x_1) (term (x_2 ... x_3 ...)))))
        "assign-local-bound")
   (--> ((in-hole E (assign (get-field
                             (obj-val x mval ... ((string_2 ref_2) ... (string_1 ref_1) (string_3 ref_3) ...))
                             string_1)
                            val_1))
         εs
         Σ)
        ((in-hole E vnone)
         εs
         (override-store Σ ref_1 val_1))
        (side-condition (not (member (term string_1) (term (string_2 ... string_3 ...)))))
        "assign-field")
   (==> (val ... r e ...)
        (r)
        (side-condition (not (val? (term r))))
        (side-condition (not (and (empty? (term (val ...))) (empty? (term (e ...))))))
        "cascade-nonval")
   with
   [(--> (in-hole P e_1) (in-hole P e_2))
    (==> e_1 e_2)]
   ))

(define new-loc
  (let ([n 0])
    (lambda () (begin (set! n (add1 n)) n))))

(define-metafunction λπ
  extend-env : ε x ref -> ε
  [(extend-env ((x_2 ref_2) ...) x_1 ref_1) ((x_2 ref_2) ... (x_1 ref_1))])

(define-metafunction λπ
  override-store : Σ ref val -> Σ
  [(override-store ((ref_2 val_2) ...) ref_1 val_1)
   ((ref_2 val_2) ... (ref_1 val_1))
   (side-condition (not (member (term ref_1) (term (ref_2 ...)))))]
  [(override-store ((ref_2 val_2) ... (ref_1 val_0) (ref_3 val_3) ...) ref_1 val_1)
   ((ref_2 val_2) ... (ref_1 val_1) (ref_3 val_3) ...)
   (side-condition (not (member (term ref_1) (term (ref_2 ...)))))])

;; simply use this subst function for now
(define-metafunction λπ
  subst : (x ..._1) (any ..._1) e -> e
  [(subst (x ..._1) (any ..._1) e)
   ,(subst/proc (redex-match? λπ x) (term (x ...)) (term (any ...)) (term e))])

(define val? (redex-match? λπ val))

(define-term mt-env (()))
(define-term mt-store ())
