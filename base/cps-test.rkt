#lang plai-typed/untyped

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

(test (cps (CSeq (CStr "foo") (CStr "bar")))
      (pylam (K R E B C)
        (pyapp
          (pylam (K R E B C)
            (pyapp (Id K) (CStr "foo")))
          (pylam (V) ;; note that V is not used
            (pyapp
              (pylam (K R E B C) (pyapp (Id K) (CStr "bar")))
              Ki Ri Ei Bi Ci))
          Ri Ei Bi Ci)))

(test (cps (CAssign (CId 'x (GlobalId)) (CStr "woo")))
      (pylam (K R E B C)
        (pyapp
          (pylam (K R E B C)
            (pyapp (Id K) (CStr "woo")))
          (pylam (V)
            (pyapp (Id K) (CAssign (CId 'x (GlobalId)) Vi)))
          Ri Ei Bi Ci)))

(test (cps (CReturn (CStr "foo")))
      (pylam (K R E B C)
        (pyapp
          (pylam (K R E B C) (pyapp (Id K) (CStr "foo")))
          Ri Ri Ei Bi Ci)))
          
(test (cps (CReturn (CReturn (CStr "foo"))))
      (pylam (K R E B C)
        (pyapp
          (pylam (K R E B C)
                  (pyapp
                    (pylam (K R E B C) (pyapp (Id K) (CStr "foo")))
                    Ri Ri Ei Bi Ci))
          Ri Ri Ei Bi Ci)))

;; It's a shame this doesn't work; testing full on interpretation will
;; be more involved...
#;(test (cps-eval (CSeq (CStr "foo") (CStr "bar")))
      (CStr "bar"))

