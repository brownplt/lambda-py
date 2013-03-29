#lang racket
(require "lambdapy-test-util.rkt")

  (full-expect
   (,(redex-str "test-string")
    {(%str 1)}
    {(1 vnone)})
   ((pointer-val 2)
    {(%str 1)}
    {(1 (obj-val %none (meta-none) ()))
     (2 (obj-val %str (meta-str "test-string") ()))}))
  
  (full-expect
   ((get-attr (object (id %str global) (meta-str "foo"))
              ,(redex-str "inherited"))
    {(%str 5)}
    {(4 (obj-val %type (meta-class %str) (("__mro__" 9) ("inherited" 6))))
     (5 (pointer-val 4))
     (6 (pointer-val 7))
     (7 (obj-val %str (meta-str "should be looked up") ()))
     (9 (pointer-val 10))
     (10 (obj-val %tuple (meta-tuple ((pointer-val 4))) ()))})
   ((pointer-val 7)
    ε Σ))
  
  (full-expect
   ((get-attr (object (id %str global) (meta-str "shadowed-test"))
              ,(redex-str "shadowed"))
    {(%str 5)}
    {(4 (obj-val type (meta-class %str) (("__mro__" 9) ("shadowed" 6))))
     (5 (pointer-val 4))
     (6 (pointer-val 7))
     (7 (obj-val %str (meta-str "should be looked up") ()))
     (8 (obj-val %object (no-meta) (("shadowed" 9))))
     (9 (pointer-val 10))
     (10 (obj-val %tuple (meta-tuple ((pointer-val 4) (pointer-val 8))) ()))})
   ((pointer-val 7)
    ε Σ))
  
  (full-expect
   ((get-attr (object (id %str global) (meta-str "fetch-inherited"))
              ,(redex-str "inherited"))
    {(%str 5)}
    {(4 (obj-val type (meta-class %str) (("__mro__" 9) ("not-inherited" 6))))
     (5 (pointer-val 4))
     (6 (pointer-val 7))
     (7 (obj-val %str (meta-str "should not be looked up") ()))
     (8 (obj-val %object (no-meta) (("inherited" 9))))
     (9 (pointer-val 10))
     (10 (obj-val %tuple (meta-tuple ((pointer-val 4) (pointer-val 8))) ()))})
   ((pointer-val 10)
    ε Σ))
  
  
  (full-expect
   ((get-attr (object (id %str global) (meta-str "function-lookup"))
              ,(redex-str "inherited"))
    {(%str 5)}
    ,inherit-Σ)
   ((pointer-val ref_func)
    ε ((ref val) ...
       (ref_func (obj-val %function (meta-closure (λ (self) (no-var) (obj-val %none (meta-none) ()))) ()))
       (ref_rest val_rest) ...)))
  
  
  (full-expect
   ((object (id %str global) (meta-str "just-object-creation"))
    {(%str 1)}
    {(0 (obj-val %type (meta-class str) ()))
     (1 (pointer-val 0))})
   ((pointer-val ref_1)
    {(%str 1)}
    {(0 (obj-val %type (meta-class str) ()))
     (1 (pointer-val 0))
     (ref_1 (obj-val (pointer-val 0) (meta-str "just-object-creation") ()))}))
  
  
  (expect-raw
   (core->redex (CObject (CSym 'foo) (some (MetaStr "bar"))))
   (pointer-val ref))
  
  (full-expect
   (,(core->redex (CGetAttr (CObject (CId '%str (GlobalId)) (some (MetaStr "get-function"))) (core-str "inherited")))
    {(%str 5)}
    ,inherit-Σ)
   ((pointer-val ref_func)
    ε ((ref val) ... (ref_func (obj-val %function (meta-closure (λ (self) (no-var) (obj-val %none (meta-none) ()))) ()))
                 (ref_rest val_rest) ...)))
  
  (expect-raw
   (core->redex (CSeq (CSym 'foo) (CSym 'bar)))
   (sym "bar"))
  
  (full-expect
 (,(core->redex (CLet 'x (LocalId) (CObject (CNone) (some (MetaStr "foo")))
                      (CSeq
                       (CSetAttr (CId 'x (LocalId)) (core-str "updated1") (CObject (CNone) (some (MetaStr "val"))))
                       (CGetAttr (CId 'x (LocalId)) (core-str "updated1")))))
  ({%str 1}) ({1 vnone}))
 ((pointer-val ref)
  ε
  ((ref_1 val_1) ...
   (ref (obj-val any_cls (meta-str "val") ()))
   (ref_n val_n) ...)))

(full-expect
 (,(core->redex (CLet 'x (LocalId) (CObject (CNone) (some (MetaStr "foo")))
                      (CSeq
                       (CSetAttr (CId 'x (LocalId)) (core-str "updated2") (CObject (CNone) (some (MetaStr "val"))))
                       (CSeq
                       (CSetAttr (CId 'x (LocalId)) (core-str "updated2") (CObject (CNone) (some (MetaStr "val-2"))))
                       (CGetAttr (CId 'x (LocalId)) (core-str "updated2"))))))
  ({%str 1}) ({1 vnone}))
 ((pointer-val ref)
  ε
  ((ref_1 val_1) ...
   (ref (obj-val any_cls (meta-str "val-2") ()))
   (ref_n val_n) ...)))