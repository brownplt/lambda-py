#lang plai-typed/untyped

(require
  "util.rkt"
  "builtins/num.rkt"
  "builtins/str.rkt"
  "python-lib.rkt"
  "python-core-syntax.rkt"
  "python-interp.rkt"
  "python-lib.rkt"
  "util.rkt"
  "python-cps.rkt")


(test (pylam ('k) (CId 'x (LocalId)))
      (CFunc (list 'k) (none)
        (CReturn (CId 'x (LocalId)))
        (none)))

(test (cps (CId 'x (LocalId)))
      (pylam (K R E B C)
        (pyapp (Id K) (CId 'x (LocalId)))))

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

(test (cps (CReturn (CReturn (CSym 'foo))))
      (pylam (K R E B C)
        (pyapp
          (pylam (K R E B C)
            (pyapp
              (pylam (K R E B C) (pyapp (Id K) (CSym 'foo)))
              Ri Ri Ei Bi Ci))
          Ri Ri Ei Bi Ci)))

(test (cps (CRaise (some (CSym 'bar))))
      (pylam (K R E B C)
        (pyapp
          (pylam (K R E B C)
            (pyapp Ki (CSym 'bar)))
          Ei Ri Ei Bi Ci)))

#|
(test (cps (CBuiltinPrim 'num+ (CStr "as") (CStr "foo") (CStr "bar")))
  (pylam (K R E B C)
    (pyapp
      (pylam (K R E B C) (pyapp Ki (CStr "as")))
      (pylam (V)
        (pyapp
          (pylam (K R E B C) (pyapp Ki (CStr "foo")))
          (pylam (V2)
            (pyapp
              (pylam (K R E B C) (pyapp Ki (CStr "bar")))
              (pyapp Ki (CBuiltinPrim 'num+ Vi V2i
              |#

#;(test (cps (CWhile (CTrue) (CStr "body") (CStr "else")))
      (pylam (K R E B C)
        (Let '-while (CSym 'nothing)
        (Let '-continue (CSym 'nothing)
        (Let '-elsethunk
          (pylam ()
            (pyapp
              (pylam (K R E B C)
                (pyapp Ki (CSym 'else)))
              Ki Ri Ei Bi Ci))
            (CSeq
              (CAssign (Id '-while)
                (pylam (K R E B C)
                  (pyapp
                    (pylam (K R E B C)
                      (pyapp Ki (CTrue)))
                    (pylam (V)
                     (CIf
                       Vi
                       (pyapp
                        (pylam (K R E B C)
                          (pyapp Ki (CSym 'body)))
                        (pylam (V)
                          (pyapp (Id '-while) Ki Ri Ei Bi Ci))
                        Ri Ei Bi Ci)
                       (pyapp (Id '-elsethunk))))
                     Ri Ei Bi Ci)))
            (CSeq
              (CAssign '-continue
                (pylam (V)
                  (pyapp
                   (Id '-while)
                   Ki
                   Ri
                   Ei
                   (pylam (V) (pyapp (Id '-elsethunk)))
                   (Id '-continue))))
              (pyapp (Id 'continue) (CSym 'nothing)))))))))

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

(test (cps-eval (CWhile (CSym 'false) (CSym 'body) (CSym 'else)))
      (VSym 'else))

;; NOTE(dbp): not working right now
;;(test (cps-eval (pyapp (gid 'print) (CSym 'foo)))
;;      (VObjectClass 'none (some (MetaNone)) (hash empty) (none)))

(define fun-expr (CApp (CFunc (list) (some 'arg)
                               ;; BUG(dbp)? stararg wrapped in extra tuple
                               (CReturn (pyget (Id 'arg) (make-builtin-num 0)))
                               (none))
                       (list)
                       (some (CTuple (gid '%tuple) (list (CSym 'foo-starred))))))

(test (cps-eval fun-expr)
      (VSym 'foo-starred))

(define tuple-field-expr (pyget (CTuple (gid '%tuple) (list (CSym 'foo)))
                                 (make-builtin-num 0)))
(test (cps-eval tuple-field-expr)
      (VSym 'foo))

(test (cps-eval (pyget (CList (gid '%list) (list (CSym 'foo) (CSym 'bar)))
                       (make-builtin-num 1)))
      (VSym 'bar))

(test (cps-eval
  (Let 'x (make-builtin-num 0)
    (CWhile (CTrue)
      (CIf
       (CBuiltinPrim 'num> (list (Id 'x) (make-builtin-num 10)))
       (CBreak)
       (CAssign (Id 'x) (CBuiltinPrim 'num+ (list (Id 'x) (make-builtin-num 2)))))
      (CSym 'finished))))
  (VObjectClass 'none (some (MetaNone)) (hash empty) (none)))

(test (MetaStr-s (some-v (VObjectClass-mval
       (cps-eval (CSeq (make-builtin-str "foo") (make-builtin-str "bar"))))))
      "bar")



(test (MetaSet-elts (some-v (VObjectClass-mval
       (cps-eval (CSet (gid '%set) (list (CSym 'foo) (CSym 'bar)))))))
      (make-set (list (VSym 'foo) (VSym 'bar))))

;; NOTE(dbp): not sure how to test tryfinally properly, because we
;; don't have a way to capture that the finally block was run, as we
;; aren't testing side effects. Also, current test framework wants
;; only actual values to be the result, so writing tests that raise
;; things or have breaks/continues causes errors... errg. Perhaps this
;; will be better served by larger scale tests where all of those things
;; can be asserted.
(test (cps-eval (CTryFinally (CSym 'foo) (CSym 'bar)))
      (VSym 'foo))


(test (cps-eval (CTryExceptElse (CRaise (some (CSym 'foo)))
                                'exn
                                (Id 'exn)
                                (CSym 'bar)))
      (VSym 'foo))

(test (cps-eval (CTryExceptElse (CSym 'foo)
                                'exn
                                (Id 'exn)
                                (CSym 'bar)))
      (VSym 'bar))

(test (MetaStr-s (some-v (VObjectClass-mval
      (cps-eval (CTryExceptElse
                 (CTryExceptElse (CRaise (some (make-builtin-str "foo")))
                                 'exn
                                 (CRaise (none))
                                 (CSym 'bar))
                 'exn
                 (Id 'exn)
                 (CSym 'goo))))))
      "foo")
