#lang racket

(require "lambdapy-test-util.rkt")

(full-expect
 ((let (x local = (undefined-val)) in
    (id x local)) () ())
 ((err val_err) ε Σ))

(full-expect
 ((let (x local = (undefined-val)) in
    (seq
     (assign (id x local) := (sym "testval"))
     (id x local)))
  ()
  ())
 ((sym "testval") ε Σ))

(full-expect
 ((let (x local = (undefined-val)) in
    (seq
     (assign (id x local) := (sym "testval"))
     (seq
      (delete (id x local))
      (id x local))))
  ()
  ())
 ((err val_err) ε Σ))

(full-expect
 ((delete (id x global))
  {(x 5)} {(5 vnone)})
 (val_none () {(5 val_none)}))

(when (redex-match λπ e (term (undefined-val)))
  (error 'redex-tests "Undefined-val was an expression"))

(when (not (redex-match λπ e+undef (term (undefined-val))))
  (error 'redex-tests "Undefined-val was an expression"))

(when (redex-match λπ (get-attr e e) (term (get-attr (undefined-val) vnone)))
  (error 'redex-tests "Undefined-val was a (nested) expression"))
