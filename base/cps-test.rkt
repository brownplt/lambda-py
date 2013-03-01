#lang plai-typed

(require
  "python-core-syntax.rkt"
  "util.rkt"
  "python-cps.rkt")

(test (pylam ('k) (CId 'x (LocalId)))
      (CFunc (list 'k) (none)
        (CId 'x (LocalId))
        (none)))

(test (cps (CId 'x (LocalId)))
      (CFunc (list K R E B C) (none)
        (CApp (Id K) (list (CId 'x (LocalId))) (none))
        (none)))

(test (cps (CSeq (make-builtin-str "foo") (make-builtin-str "bar")))
      (pylam (K R E B C)
        (pyapp
          (pylam (K R E B C)
            (pyapp (Id K) (make-builtin-str "foo")))
          (pylam (V) ;; note that V is not used
            (pyapp
              (pylam (K R E B C) (pyapp (Id K) (make-builtin-str "bar")))
              Ki Ri Ei Bi Ci))
          Ri Ei Bi Ci)))

(test (cps (CAssign (CId 'x (GlobalId)) (make-builtin-str "woo")))
      (pylam (K R E B C)
        (pyapp
          (pylam (K R E B C)
            (pyapp (Id K) (make-builtin-str "woo")))
          (pylam (V)
            (pyapp (Id K) (CAssign (CId 'x (GlobalId)) Vi)))
          Ri Ei Bi Ci)))

(test (cps (CReturn (make-builtin-str "foo")))
      (pylam (K R E B C)
        (pyapp
          (pylam (K R E B C) (pyapp (Id K) (make-builtin-str "foo")))
          Ri Ri Ei Bi Ci)))
          
(test (cps (CReturn (CReturn (make-builtin-str "foo"))))
      (pylam (K R E B C)
        (pyapp
          (pylam (K R E B C)
                  (pyapp
                    (pylam (K R E B C) (pyapp (Id K) (make-builtin-str "foo")))
                    Ri Ri Ei Bi Ci))
          Ri Ri Ei Bi Ci)))

;; It's a shame this doesn't work; testing full on interpretation will
;; be more involved...
#;(test (cps-eval (CSeq (make-builtin-str "foo") (make-builtin-str "bar")))
      (make-builtin-str "bar"))

