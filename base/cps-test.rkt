#lang plai-typed/untyped

(require
  "util.rkt"
  "python-lib.rkt"
  "python-core-syntax.rkt"
  "python-cps.rkt")

(test (pylam ('k) (CId 'x (LocalId)))
      (CFunc (list 'k) (none)
        (CReturn (CId 'x (LocalId)))
        (none)))

(test (cps (CId 'x (LocalId)))
      (CFunc (list K R E B C) (none)
        (CReturn (CApp (Id K) (list (CId 'x (LocalId))) (none)))
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

(test (cps (CRaise (CStr "bar")))
      (pylam (K R E B C)
        (pyapp
          (pylam (K R E B C)
            (pyapp Ki (CStr "bar")))
          Ei Ri Ei Bi Ci)))

#;(test (cps (CWhile (CTrue) (CStr "body") (CStr "else")))
      (pylam (K R E B C)
        (CLet '-while (CNone)
          (CSeq
            (CAssign (Id '-while)
              (pylam (K R E B C)
                (pylam (K R E B C)
                  (pyapp Ki (CTrue)))
                (pylam (V)
                 (CIf
                   Vi
                   (pyapp
                    (pylam (K R E B C)
                      (pyapp Ki (CStr "body")))
                    (pylam (V)
                      (pyapp (Id '-while) Ki Ri Ei Bi Ci))
                    Ri Ei Bi Ci)
                   (pyapp
                    (pylam (K R E B C)
                      (pyapp Ki (CStr "else")))
                    ;; TODO(joe): should these be the
                    ;; outer bindings for the else block?
                    Ki Ri Ei Bi Ci)))
                 Ri Ei Bi Ci))
            ;; TODO(joe): Bi/Ci special
            (pyapp (Id '-while) Ki Ri Ei Bi Ci)))))

;; TODO(joe): I thinks some logic from interp needs to
;; go to desugar to make this work with excepts, etc.
#;(test (cps (CTryExceptElseFinally (CRaise (CStr "bar"))
              (list (CExcept
                     (list (Id 'Exception))
                     (some 'e)
                     (CStr "caught")))
              (CNone)
              (CNone)))
  (pylam (K R E B C)
    (pyapp
     (pylam (K R E B C)
       (pyapp
        (pylam (K R E B C)
          (pyapp Ki (CStr "bar")))
        Ei Ri Ei Bi Ci)) ;; NOTE(joe): Ei not Ki for exception
     Ki Ri
     (pylam (V)
       (pyapp
        (pylam (K R E B C) (pyapp Ki (CStr "caught")))
        Ki Ri Ei Bi Ci))
     Bi Ci)))

(test (cps-eval (CSeq (CSym 'foo) (CSym 'bar)))
      (VSym 'bar))

