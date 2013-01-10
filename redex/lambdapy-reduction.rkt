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
   (==> true
	vtrue
	"true")
   (==> false
	vfalse
	"false")
   (==> (list (val ...))
        (obj-val list (meta-list (val ...)) ())
        "list")
   (==> (tuple (val ...))
        (obj-val tuple (meta-tuple (val ...)) ())
        "tuple")
   (==> (set (val ...))
        (obj-val set (meta-set (val ...)) ())
        "set")
   (==> (dict ((val_1 val_2) ...))
        (obj-val $dict (meta-dict ((val_1 val_2) ...)) ())
        "dict") ;; TODO: duplicated keys
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
   (--> ((in-hole E (prim2 op val_1 val_2)) εs Σ)
        ((in-hole E (δ op val_1 val_2 εs Σ)) εs Σ)
        "prim2")
   (--> ((in-hole E (builtin-prim op (val ...))) εs Σ)
        ((in-hole E (δ op val ... εs Σ)) εs Σ)
        "builtin-prim")
   (==> (if val e_1 e_2)
        e_1
        (side-condition (term (truthy? val)))
        "if-true")
   (==> (if val e_1 e_2)
        e_2
        (side-condition (not (term (truthy? val))))
        "if-false")
   ;; NOTE(yao): this may be unnecessary, since context T deals with it
   ;; I wrote it and then deleted.
   ;; same thing for "seq-return" etc., if we use context to do it
   #|
   (==> (if (exception-r val) e_1 e_2)
	(exception-r val)
	"if-exception")|#
   (==> (seq val e)
        e
        "seq")
   (==> (seq (return-r val) e)
	(return-r val)
	"seq-return")
   #|
   (==> (seq break-r e)
	break-r
	"seq-break")
   (==> (seq (exception-r val) e)
	(exception-r val)
	"seq-exception")|#
   (==> (while e_1 e_2 e_3)
        (loop (if e_1 (seq e_2 (while e_1 e_2 e_3)) e_3)) ;; not handle break yet
        "while")
   (==> (loop val)
        vnone
        "loop")
   (==> (loop (in-hole H break-r))
        vnone
        "loop-break")
   (==> break
        break-r
        "break")
   (==> (return val)
	(return-r val)
	"return")
   (==> (raise val)
        (exception-r val)
        "raise") ;; TODO: check type of val
   (==> (try val (e_exc ...) val_else e_finally)
        e_finally
        "try-noexc")
   (==> (try val (e_exc ...) e_else e_finally)
        (try e_else () vnone e_finally)
        (side-condition (not (val? (term e_else))))
        "try-else")
   (==> (try (in-hole T (exception-r val)) () e_else e_finally)
        (seq e_finally (exception-r val))
        "try-exc-nohandler")
   (==> (try (in-hole T (exception-r val)) ((except () e) e_exc ...) e_else e_finally)
        (try e () vnone e_finally)
        "try-exc-notype")
   (==> (try (in-hole T (exception-r val)) ((except () (x) e) e_exc ...) e_else e_finally)
        (try (let (x val) e) () vnone e_finally)
        "try-exc-notype-named")
   (==> (try (in-hole T (exception-r val)) ((except (e_type1 e_type ...) e) e_exc ...) e_else e_finally)
        (try (if (builtin-prim "isinstance" (val e_type1)) ;; in principle "isinstance" should handle tuple (tuple (e_type1 e_type ...))
                 e
                 (try (in-hole T (exception-r val)) (e_exc ...) e_else vnone))
             () vnone e_finally)
        "try-exc-type")
   (==> (try (in-hole T (exception-r val)) ((except (e_type1 e_type ...) (x) e) e_exc ...) e_else e_finally)
        (try (if (builtin-prim "isinstance" (val e_type1)) ;; in principle "isinstance" should handle tuple (tuple (e_type1 e_type ...))
                 (let (x val) e)
                 (try (in-hole T (exception-r val)) (e_exc ...) e_else vnone))
             () vnone e_finally)
        "try-exc-type-named")
   (==> (try (in-hole T r) (e_exc ...) e_else e_finally)
        (seq e_finally r)
        (side-condition (not (val? (term r))))
        (side-condition (not (redex-match? λπ (exception-r any) (term r))))
        "try-nonval")
   ;; NOTE(dbp): I don't think this is the correct behavior - uncaught exceptions
   ;; should percolate up as (exception-r val) results, NOT cause racket errors.
   ;;    agreed. (exception-r val) should be the reduction result. -yao
   (--> ((in-hole T (exception-r val)) εs Σ)
        ((exception-r val) εs Σ)
        (side-condition (not (redex-match? λπ hole (term T)))) ;; avoid cycle
        "exc-uncaught")
   (==> (module val e)
        e
        "module")
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
   (--> ((in-hole E (let (x_1 break-r) e))
         (ε_1 ε ...)
         Σ)
        ((in-hole E e)
         ((extend-env ε_1 x_1 ref_1) ε ...)
         (override-store Σ ref_1 vnone))
        (where ref_1 ,(new-loc))
        "let-brk")
   #|
   (==> (let (x_1 (exception-r val)) e_1)
        (exception-r val)
        "let-exc")|#
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
   (--> ((in-hole E (get-field (obj-val x_base mval ... ((string_2 ref_2) ...)) string_1))
         (name env (((x_1 ref_x1) ...) ... ((x_2 ref_x2) ... (x_base ref_base) (x_3 ref_x3) ...) ε ...))
         (name store ((ref_4 val_4) ... (ref_base val_base) (ref_5 val_5) ...)))
        ((in-hole E (get-field val_base string_1))
         env
         store)
        (side-condition (not (member (term string_1) (term (string_2 ...)))))
        (side-condition (not (member "__class__" (term (string_2 ...)))))
        (side-condition (not (member (term x_base) (append* (term ((x_1 ...) ...))))))
        (side-condition (not (member (term x_base) (term (x_2 ... x_3 ...)))))
        (side-condition (not (member (term ref_base) (term (ref_4 ... ref_5 ...)))))
        "get-field-base")
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
