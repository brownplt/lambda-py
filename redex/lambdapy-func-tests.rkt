#lang racket
(require "lambdapy-test-util.rkt")

(full-expect
 (,(core->redex (CApp (CFunc '(identity) (none) (CReturn (CId 'identity (LocalId))) (none))
                      (list (CObject (CId '%str (GlobalId))
                                     (some (MetaStr "identity function"))))
                      (none)))
  ,inherit-ε
  ,inherit-Σ)
 ((pointer-val ref_str)
  ε
  {(ref val) ... (ref_str (obj-val (pointer-val ref_cls)
                                   (meta-str "identity function")
                                   ()))
   (ref_n val_n) ...}))

(full-expect
 (,(core->redex (CApp (CFunc '() (none) (CReturn (CObject (CId '%str (GlobalId))
                                                          (some (MetaStr "no args")))) (none))
                      (list)
                      (none)))
  ,inherit-ε
  ,inherit-Σ)
 ((pointer-val ref_str)
  ε
  {(ref val) ... (ref_str (obj-val (pointer-val ref_cls)
                                   (meta-str "no args")
                                   ()))
   (ref_n val_n) ...}))

(full-expect
 (,(core->redex (CApp
                 (CApp (CFunc '(x) (none)
                              (CReturn (CFunc '(y) (none) (CReturn (CId 'x (LocalId))) (none)))
                              (none))
                       (list (CObject (CId '%str (GlobalId))
                                      (some (MetaStr "close over this one"))))
                       (none))
                 (list py-none)
                 (none)))
  ,inherit-ε
  ,inherit-Σ)
 ((pointer-val ref_str)
  ε
  {(ref val) ... (ref_str (obj-val (pointer-val ref_cls)
                                   (meta-str "close over this one")
                                   ()))
   (ref_n val_n) ...}))

(full-expect
 ((app (fun () (no-var)
            (return (builtin-prim "tuple-getitem" ((fetch (id args local)) (fetch (object vnone (meta-num 0)))))))
       ((object vnone (meta-str "get-nothing"))
        (object vnone (meta-str "not-here-either"))))
  () ())
 ((err (obj-val any_cls (meta-str "arity-mismatch") any_dict)) ε Σ))

(full-expect
 ((app (fun () (args)
            (return (builtin-prim "tuple-getitem" ((fetch (id args local)) (fetch (object vnone (meta-num 0)))))))
       ((object vnone (meta-str "get-this-one(first)"))
        (object vnone (meta-str "not-this-one"))))
  () ())
 ((pointer-val ref_arg)
  ε
  ((ref_1 val_1) ...
   (ref_arg (obj-val any_cls (meta-str "get-this-one(first)") any_dict))
   (ref_n val_n) ...)))

(full-expect
 ((app (fun (x) (args)
            (return (builtin-prim "tuple-getitem" ((fetch (id args local)) (fetch (object vnone (meta-num 0)))))))
       ()
       (tuple (id %tuple global)
              ((object vnone (meta-str "not-this-one"))
               (object vnone (meta-str "get-this-one(second)")))))
  {(%tuple 4)} {(4 vnone)})
 ((pointer-val ref_arg)
  ε
  ((ref_1 val_1) ...
   (ref_arg (obj-val any_cls (meta-str "get-this-one(second)") any_dict))
   (ref_n val_n) ...)))

(full-expect
 ((app (fun (x) (no-var)
            (return (builtin-prim "tuple-getitem" ((fetch (id args local)) (fetch (object vnone (meta-num 0)))))))
       ()
       (tuple (id %tuple global)
              ((object vnone (meta-str "not-this-one"))
               (object vnone (meta-str "get-this-one")))))
  {(%tuple 4)} {(4 vnone)})
 ((err (obj-val any_cls (meta-str "arity-mismatch") any_dict))
  ε
  ((ref_1 val_1) ...
   (ref_arg (obj-val any_cls (meta-str "get-this-one") any_dict))
   (ref_n val_n) ...)))

(full-expect
 ((app (fun (x to-fetch) (no-var)
            (return (id to-fetch local)))
       ()
       (tuple (id %tuple global)
              ((object vnone (meta-str "not-this-one"))
               (object vnone (meta-str "get-this-one")))))
  {(%tuple 4)} {(4 vnone)})
 ((pointer-val ref_arg)
  ε
  ((ref_1 val_1) ...
   (ref_arg (obj-val any_cls (meta-str "get-this-one") any_dict))
   (ref_n val_n) ...)))
