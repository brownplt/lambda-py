#lang racket

(require redex)
(require "lambdapy-core.rkt")

(provide (all-defined-out))

(define-term vnone (obj-val none (meta-none) ()))
(define-term vtrue (obj-val bool (meta-num 1) ()))
(define-term vfalse (obj-val bool (meta-num 0) ()))

(define-metafunction λπ
  truthy? : val -> #t or #f
  [(truthy? (fun-val any ...)) #t]
  [(truthy? (obj-val x (meta-num number) (any ...))) ,(not (= 0 (term number)))]
  [(truthy? (obj-val x (meta-str string) (any ...))) ,(not (string=? "" (term string)))]
  [(truthy? (obj-val x (meta-list (val ...)) (any ...))) ,(not (empty? (term (val ...))))]
  [(truthy? (obj-val x (meta-tuple (val ...)) (any ...))) ,(not (empty? (term (val ...))))]
  [(truthy? (obj-val x (meta-dict ((val_1 val_2) ...)) (any ...))) ,(not (empty? (term (val_1 ...))))]
  [(truthy? (obj-val x (meta-set (val ...)) (any ...))) ,(not (empty? (term (val ...))))]
  [(truthy? (obj-val x (meta-none) (any ...))) #f]
  [(truthy? (obj-val x mval (any ...))) #t]
  [(truthy? (obj-val x (any ...))) #t]
  [(truthy? undefined-val) #f])

(define-metafunction λπ
  δ : op val ... εs Σ -> r
  [(δ "not" val εs Σ)
   ,(if (term (truthy? val)) (term vfalse) (term vtrue))]
  [(δ "print" val εs Σ)
   ,(begin (display (term val)) (display "\n") (term vnone))] ;; not sure how to do print for now
  [(δ "callable" (fun-val any ...) εs Σ)
   vtrue]
  [(δ "callable" (obj-val x (meta-class any) any) εs Σ)
   vtrue]
  [(δ "callable" val εs Σ)
   vfalse]
  [(δ "is" val_1 val_2 εs Σ)
   ,(if (eq? (term val_1) (term val_2)) (term vtrue) (term vfalse))] ;; current interp does this
  [(δ "isnot" val_1 val_2 εs Σ)
   ,(if (eq? (term val_1) (term val_2)) (term vfalse) (term vtrue))]
  [(δ "isinstance" val_1 (obj-val x (meta-class x_class) any) εs Σ)
   ,(if (term (object-is? val_1 x_class εs Σ)) (term vtrue) (term vfalse))])

(define-metafunction λπ
  object-is? : val x εs Σ -> #t or #f
  [(object-is? (obj-val no-super any ...) x εs Σ) #f]
  [(object-is? (obj-val x any ...) x εs Σ) #t]
  [(object-is? (obj-val x any ...)
               x_class
               (name env (((x_1 ref_x1) ...) ... ((x_2 ref_x2) ... (x ref) (x_3 ref_x3) ...) ε ...))
               (name store ((ref_4 val_4) ... (ref val) (ref_5 val_5) ...)))
   (object-is? val x_class env store)
   (side-condition (not (member (term x) (append* (term ((x_1 ...) ...))))))
   (side-condition (not (member (term x) (term (x_2 ... x_3 ...)))))
   (side-condition (not (member (term ref) (term (ref_4 ... ref_5 ...)))))])
