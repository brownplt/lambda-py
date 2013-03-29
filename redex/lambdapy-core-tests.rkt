#lang racket

(require "lambdapy-test-util.rkt")

(require (only-in "../base/util.rkt" seq-ops))

(define min-env (term {(True 1) (False 2) (None 3)
                       (%str 7) (%locals 8) (%globals 9)}))
(define min-sto (term {(1 (pointer-val 4))
                       (2 (pointer-val 5))
                       (3 (pointer-val 6))
                       (4 (obj-val %bool (meta-num 1) {}))
                       (5 (obj-val %bool (meta-num 2) {}))
                       (6 (obj-val %none (meta-none) {}))
                       (7 vnone)
                       (8 vnone)
                       (9 vnone)}))

(full-expect
 (,(core->redex (get-core-syntax (open-input-string "
def f(x):
  return x
f('a-str')
")))
  ,min-env
  ,min-sto)
 ((pointer-val ref_str)
  {(x ref) ... (f ref_f)}
  {(ref_1 val_1) ...
   (ref_str (obj-val any_cls (meta-str "a-str") ()))
   (ref_n val_n) ...}))

#;(full-expect
 (,(core->redex (get-core-syntax (open-input-file "../base/pylib/none.py")))
  () ())
 ((err val) ε Σ))

(full-expect
 (,(core->redex (CBuiltinPrim 'num+
                              (list 
                               (CObject (CNone) (some (MetaNum 4)))
                               (CObject (CNone) (some (MetaNum 5))))))
  ,min-env ,min-sto)
 ((pointer-val ref)
  ε
  ((ref_1 val_1) ...
   (ref (obj-val any_cls (meta-num 9) ()))
   (ref_n val_n) ...)))


(full-expect
   (,(core->redex (cascade-lets lib-function-dummies (CSym 'done)))
    ,min-env ,min-sto)
   ((sym "done") ε Σ))

#;(full-expect
 (,(core->redex (cascade-lets lib-function-dummies
                              (seq-ops (append
                                        (map (lambda (b) (bind-right b)) lib-functions)
                                        (list (CSym 'done))
                                        ))))
  ,min-env ,min-sto)
 ((obj-val %bool (meta-num 1) ()) ε Σ))