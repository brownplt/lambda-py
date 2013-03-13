#lang plai-typed/untyped

(require
  "util.rkt"
  "builtins/num.rkt"
  "python-lib.rkt"
  "python-core-syntax.rkt"
  "python-interp.rkt"
  "python-lib.rkt"
  "util.rkt"
  "python-cps.rkt")

(display (pylam ('k) (CId 'x (LocalId))))

(test (pylam ('k) (CId 'x (LocalId)))
      (CFunc (list 'k) (none)
        (CReturn (CId 'x (LocalId)))
        (none)))

(test (cps (CId 'x (LocalId)))
      (CFunc (list K R E B C) (none)
        (CReturn (CApp (Id K) (list (CId 'x (LocalId))) (none)))
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

(test (cps (CReturn (CReturn (CSym 'foo))))
      (pylam (K R E B C)
        (pyapp
          (pylam (K R E B C)
            (pyapp
              (pylam (K R E B C) (pyapp (Id K) (CSym 'foo)))
              Ri Ri Ei Bi Ci))
          Ri Ri Ei Bi Ci)))

(test (cps (CRaise (CSym 'bar)))
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

(test (cps-eval (pyapp (gid 'print) (CSym 'foo)))
      (VObjectClass 'none (some (MetaNone)) (hash empty) (none)))

(define fun-expr (CApp (CFunc (list) (some 'arg)
			      (CReturn (Id 'arg))
			      (none))
		       (list)
		       (some (CTuple (list (make-builtin-str "foo-starred"))))))

(test (VObjectClass-mval
       (first (MetaTuple-v (some-v (VObjectClass-mval
		       (cps-eval fun-expr))))))
      (some (MetaStr "foo-starred")))

(define tuple-field-expr (pyget (CTuple (list (CSym 'foo)))
				(make-builtin-num 0)))
(test (cps-eval tuple-field-expr)
      (VSym 'foo))

;; TODO(joe): need builtin prim (list cps) for this to work.
(test (cps-eval
  (Let 'x (make-builtin-num 0)
    (CWhile (CTrue)
      (CSeq
        (CNone);(CPrim1 'print (Id 'x))
        (CIf
	  (CSeq
	   (pyapp (gid 'print) (CBuiltinPrim 'num> (list (Id 'x) (make-builtin-num 10))))
	   (CSeq (pyapp (gid 'print) (Id 'x))
          (CBuiltinPrim 'num> (list (Id 'x) (make-builtin-num 10)))))
          (CBreak)
          (CSeq
            (CNone);(CPrim1 'print (Id 'x))
            (CAssign (Id 'x) (CBuiltinPrim 'num+ (list (Id 'x) (make-builtin-num 2)))))))
      (CSym 'finished))))
  (VObjectClass 'none (none) (hash empty) (none)))

#;(test (cps-eval (CWhile (CSym 'true) (CSym 'body) (CSym 'else)))
      (VSym 'body))
          
#;(test (cps (CReturn (CReturn (make-builtin-str "foo"))))
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

