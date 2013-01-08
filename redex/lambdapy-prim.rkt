#lang racket

(require redex)
(require "lambdapy-core.rkt")

(provide (all-defined-out))

(define-term vnone (obj-val none (meta-none) ()))
(define-term vtrue (obj-val bool (meta-num 1) ()))
(define-term vfalse (obj-val bool (meta-num 0) ()))

(define-metafunction λπ
  mknum : number -> val
  [(mknum number) (obj-val ,(if (exact? (term number)) (term int) (term float))
			   (meta-num number)
			   ())])

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
   ,(if (term (object-is? val_1 x_class εs Σ)) (term vtrue) (term vfalse))]
  ;; numbers, no type-checking yet
  [(δ "num+" (obj-val x_1 (meta-num number_1) any_1) (obj-val x_2 (meta-num number_2) any_2) εs Σ)
   ,(make-num (+ (term number_1) (term number_2)))]
  [(δ "num-" (obj-val x_1 (meta-num number_1) any_1) (obj-val x_2 (meta-num number_2) any_2) εs Σ)
   ,(make-num (- (term number_1) (term number_2)))]
  [(δ "num*" (obj-val x_1 (meta-num number_1) any_1) (obj-val x_2 (meta-num number_2) any_2) εs Σ)
   (obj-val num (meta-num ,(* (term number_1) (term number_2))) ())]
  [(δ "num*" (name v1 (obj-val x_1 (meta-num number_1) any_1)) (name v2 (obj-val x_2 (meta-str string_2) any_2)) εs Σ)
   (δ "str*" v2 v1 εs Σ)]
  [(δ "num/" (obj-val x_1 (meta-num number_1) any_1) (obj-val x_2 (meta-num number_2) any_2) εs Σ)
   (obj-val num (meta-num ,(/ (term number_1) (term number_2))) ())]
  [(δ "num//" (obj-val x_1 (meta-num number_1) any_1) (obj-val x_2 (meta-num number_2) any_2) εs Σ)
   (obj-val num (meta-num ,(quotient (term number_1) (term number_2))) ())]
  [(δ "num%" (obj-val x_1 (meta-num number_1) any_1) (obj-val x_2 (meta-num number_2) any_2) εs Σ)
   (obj-val num (meta-num ,(remainder (term number_1) (term number_2))) ())]
  [(δ "num=" (obj-val x_1 (meta-num number_1) any_1) (obj-val x_2 (meta-num number_2) any_2) εs Σ)
   ,(if (= (term number_1) (term number_2)) (term vtrue) (term vfalse))]
  [(δ "num>" (obj-val x_1 (meta-num number_1) any_1) (obj-val x_2 (meta-num number_2) any_2) εs Σ)
   ,(if (> (term number_1) (term number_2)) (term vtrue) (term vfalse))]
  [(δ "num<" (obj-val x_1 (meta-num number_1) any_1) (obj-val x_2 (meta-num number_2) any_2) εs Σ)
   ,(if (< (term number_1) (term number_2)) (term vtrue) (term vfalse))]
  [(δ "num>=" (obj-val x_1 (meta-num number_1) any_1) (obj-val x_2 (meta-num number_2) any_2) εs Σ)
   ,(if (>= (term number_1) (term number_2)) (term vtrue) (term vfalse))]
  [(δ "num<=" (obj-val x_1 (meta-num number_1) any_1) (obj-val x_2 (meta-num number_2) any_2) εs Σ)
   ,(if (<= (term number_1) (term number_2)) (term vtrue) (term vfalse))]
  [(δ "numcmp" (obj-val x_1 (meta-num number_1) any_1) (obj-val x_2 (meta-num number_2) any_2) εs Σ)
   ,(cond
      [(< (term number_1) (term number_2)) (term (obj-val num (meta-num -1) ()))]
      [(> (term number_1) (term number_2)) (term (obj-val num (meta-num 1) ()))]
      [else (term (obj-val num (meta-num 0) ()))])]
  [(δ "num-str" (obj-val x (meta-num number) any) εs Σ)
   (obj-val str (meta-str ,(number->string (term number))) ())]
  ;; strings, no type-checking yet
  [(δ "str+" (obj-val x_1 (meta-str string_1) any_1) (obj-val x_2 (meta-str string_2) any_2) εs Σ)
   (obj-val str (meta-str ,(string-append (term string_1) (term string_2))) ())]
  [(δ "str=" (obj-val x_1 (meta-str string_1) any_1) (obj-val x_2 (meta-str string_2) any_2) εs Σ)
   ,(if (string=? (term string_1) (term string_2)) (term vtrue) (term vfalse))]
  [(δ "str*" (obj-val x_1 (meta-str string_1) any_1) (obj-val x_2 (meta-num number_2) any_2) εs Σ)
   (obj-val str (meta-str ,(string-append* (build-list (term number_2) (lambda (n) (term string_1))))) ())]
  [(δ "strcmp" (obj-val x_1 (meta-str string_1) any_1) (obj-val x_2 (meta-str string_2) any_2) εs Σ)
   ,(cond
      [(string<? (term string_1) (term string_2)) (term (obj-val num (meta-num -1) ()))]
      [(string>? (term string_1) (term string_2)) (term (obj-val num (meta-num 1) ()))]
      [else (term (obj-val num (meta-num 0) ()))])]
  [(δ "strlen" (obj-val x (meta-str string) any) εs Σ)
   (obj-val num (meta-num ,(string-length (term string))) ())]
  [(δ "strbool" (obj-val x (meta-str string) any) εs Σ)
   ,(if (string=? (term string) "") (term vfalse) (term vtrue))]
  [(δ "strmin" (obj-val x (meta-str string) any) εs Σ)
   (obj-val str (meta-str string_new) ())
   (where string_new ,(make-string 1
                                   (integer->char
                                    (foldl (lambda (c res)
                                             (min res c))
                                           ;; the maximum char integer is currently #x10FFFF
                                           ;; should find a better way to do this
                                           #x110000
                                           (map char->integer
                                                (string->list (term string)))))))]
  [(δ "strmax" (obj-val x (meta-str string) any) εs Σ)
   (obj-val str (meta-str string_new) ())
   (where string_new ,(make-string 1
                                   (integer->char
                                    (foldl (lambda (c res)
                                             (max res c))
                                           -1
                                           (map char->integer
                                                (string->list (term string)))))))]
  )

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

(define (make-num n)
  (term (obj-val ,(if (exact? n) (term int) (term float))
                 (meta-num ,n)
                 ())))
