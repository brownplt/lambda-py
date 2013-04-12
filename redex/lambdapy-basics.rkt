#lang racket

(require "lambdapy-test-util.rkt")

;; object values
(full-expect
 ((list vnone (vtrue vfalse)) () ())
 ((pointer-val 1)
  ()
  ((1 (obj-val val_none (meta-list (val_true val_false)) ())))))

;; control flow
(expect (if (raise vtrue) vfalse vfalse)
        (err vtrue))
(expect (seq vtrue vfalse) vfalse)
(expect (seq (raise vtrue) vfalse)
        (err vtrue))
(expect (seq vfalse (raise vtrue))
        (err vtrue))
(expect (while vtrue break vfalse)
        vnone)


;; binding
(expect (let (x local = vtrue) in (id x local))
        vtrue)
(expect (let (x local = (raise vtrue)) in vfalse)
        (err vtrue))
(expect (let (x local = vtrue) in (raise vfalse))
        (err vfalse))

(expect
 (builtin-prim "list-getitem" (
                               (obj-val %list (meta-list ((pointer-val 0) (pointer-val 1))) ())
                               (obj-val %int (meta-num 0) ())))
 (pointer-val 0))

(expect
 (builtin-prim "list-getitem" (
                               (fetch (list vnone ((pointer-val 0) (pointer-val 1))))
                               (fetch (object vnone (meta-num 0)))))
 (pointer-val 0))

(full-expect
 ((id str global) ((str 1)) ((1 (obj-val %str (meta-str "just-var-lookup") ()))))
 ((obj-val %str (meta-str "just-var-lookup") ())  ((str 1)) ((1 (obj-val %str (meta-str "just-var-lookup") ())))))
