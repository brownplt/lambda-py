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
   ,(begin (display (term val)) (term vnone))] ;; not sure how to do print for now
  [(δ "callable" (fun-val any ...) εs Σ)
   vtrue]
  [(δ "callable" (obj-val x (meta-class any) any) εs Σ)
   vtrue]
  [(δ "callable" val εs Σ)
   vfalse]
  [(δ "is" val_1 val_2 εs Σ)
   ,(if (eq? (term val_1) (term val_2)) (term vtrue) (term vfalse))] ;; current interp does this
  [(δ "isnot" val_1 val_2 εs Σ)
   ,(if (eq? (term val_1) (term val_2)) (term vfalse) (term vtrue))])
