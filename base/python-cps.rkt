#lang plai-typed/untyped

(require
  "python-core-syntax.rkt"
  "util.rkt"
  "python-interp.rkt"
  (typed-in racket/base (gensym : (symbol -> symbol))))

;; Identifiers used in the headers of CPS-generated lambdas
(define K (gensym 'next))
(define Ki (Id K))
(define R (gensym 'return))
(define Ri (Id R))
(define E (gensym 'exn))
(define Ei (Id E))
(define B (gensym 'break))
(define Bi (Id B))
(define C (gensym 'continue))
(define Ci (Id C))
(define V (gensym 'value))
(define Vi (Id V))

(define (cps expr)
  (local [
    (define (const v)
      (pylam (K R E B C) (pyapp (Id K) v)))
  ]
  (type-case CExpr expr
    [CStr (s) (const expr)]
    [CTrue () (const expr)]
    [CFalse () (const expr)]
    [CNone () (const expr)]
    [CId (x l) (const expr)]

    [CSeq (e1 e2)
     (pylam (K R E B C)
       (pyapp
        (cps e1)
        (pylam (V) (pyapp (cps e2) Ki Ri Ei Bi Ci))
        Ri Ei Bi Ci))]

    [CAssign (lhs rhs)
     (type-case CExpr lhs
       [CId (x type)
        (pylam (K R E B C)
          (pyapp
           (cps rhs)
           (pylam (V)
            (pyapp (Id K) (CAssign (CId x type) Vi)))
           Ri Ei Bi Ci))]
       [CGetField (o a) (error 'cps "Assign to object nyi")]
       [else (error 'cps "CPS: got a non-id, non-obj in CAssign")])]

    [CReturn (val)
     (pylam (K R E B C)
       (pyapp (cps val) Ri Ri Ei Bi Ci))]

    [else (error 'cps "Not handled")])))

(define (cps-eval expr)
  (interp (pyapp (cps expr)
                 (pylam (V) (Id V)) ;todo - I assume this was the intention.
                 (pylam (V) (CRaise (some (CStr "Top-level return"))))
                 (pylam (V) (CRaise (some (CStr "Top-level exception"))))
                 (pylam (V) (CRaise (some (CStr "Top-level break"))))
                 (pylam (V) (CRaise (some (CStr "Top-level continue")))))))

