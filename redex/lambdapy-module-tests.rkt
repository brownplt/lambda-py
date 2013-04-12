#lang racket

(require "lambdapy-test-util.rkt")

(full-expect
 ((construct-module (pointer-val 0))
  ()
  {(0 (obj-val %code (meta-code (foo bar) modname
                                (assign (id foo global) := (sym "test")))
               ()))})
 ((pointer-val ref_mod)
  ()
  ((ref_0 val_0) ...
   (ref_foo (sym "test"))
   (ref_rest v+undef_rest) ...
   (ref_mod (obj-val any_cls (no-meta) (("foo" ref_foo) ("bar" ref_bar)))))))