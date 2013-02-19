#lang plai-typed/untyped

(require
  "python-core-syntax.rkt"
  "python-interp.rkt"
  "python-lib.rkt"
  "util.rkt"
  (typed-in racket/base (format : (string 'a -> string)))
  (typed-in racket/base (gensym : (symbol -> symbol))))

(set-pypath "/home/joe/src/Python-3.2.3/python")

(define-syntax pylam
  (syntax-rules ()
    [(_ (arg ...) body)
     (CFunc (list arg ...) (none) (CReturn body) (none))]))

(define-syntax pyapp
  (syntax-rules ()
    [(_ fun arg ...)
     (CApp fun (list arg ...) (none))]))

(define (Id x)
  (CId x (LocalId)))
(define (Let x v e)
  (CLet x (LocalId) v e))
(define (gid x)
  (CId x (GlobalId)))

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
(define V2 (gensym 'value))
(define V2i (Id V))

(define (cps expr)
  (local [
    (define (const v)
      (pylam (K R E B C) (pyapp (Id K) v)))
  ]
  (type-case CExpr expr
    [CStr (s) (const expr)]
    [CSym (s) (const expr)]
    [CTrue () (const expr)]
    [CFalse () (const expr)]
    [CNone () (const expr)]
    [CId (x l) (const expr)]
    [CObject (c b) (const expr)]

    [CLet (x typ bind body)
     (pylam (K R E B C)
       (pyapp (cps bind)
        (pylam (V)
          (CLet x typ Vi
            (pyapp (cps body) Ki Ri Ei Bi Ci)))
        Ri Ei Bi Ci))]

    [CPrim2 (op e1 e2)
     (pylam (K R E B C)
       (pyapp (cps e1)
        (pylam (V)
          (pyapp (cps e2)
           (pylam (V2)
            (pyapp Ki (CPrim2 op Vi V2i)))
           Ri Ei Bi Ci))
        Ri Ei Bi Ci))]

    [CIf (test then els)
     (pylam (K R E B C)
       (pyapp (cps test)
        (pylam (V)
          (CIf Vi
           (pyapp (cps then) Ki Ri Ei Bi Ci)
           (pyapp (cps els) Ki Ri Ei Bi Ci)))
        Ri Ei Bi Ci))]

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
     (pylam (K R E B C) (pyapp (cps val) Ri Ri Ei Bi Ci))]

    [CRaise (val)
     (pylam (K R E B C) (pyapp (cps val) Ei Ri Ei Bi Ci))]

    [CBreak ()
     (pylam (K R E B C) (pyapp Bi (CNone)))]

    [CContinue ()
     (pylam (K R E B C) (pyapp Ci (CNone)))]

    [CWhile (test body els)
     (pylam (K R E B C)
       (Let '-while (CSym 'nothing)
       (Let '-continue (CSym 'nothing)
       (Let '-elsethunk
         (pylam ()
           (pyapp (cps els) Ki Ri Ei Bi Ci))
         (CSeq
          (CAssign (Id '-while)
            (pylam (K R E B C)
              (pyapp (cps test)
               (pylam (V)
                (CIf Vi
                  (pyapp (cps body)
                    (pylam (V) (pyapp (Id '-while) Ki Ri Ei Bi Ci))
                    Ri Ei Bi Ci)
                  (pyapp (Id '-elsethunk))))
               Ri Ei Bi Ci)))
          (CSeq
           (CAssign (Id '-continue)
            (pylam (V)
              (pyapp
               (Id '-while)
               Ki Ri Ei
               (pylam (V) (pyapp (Id 'elsethunk)))
               (Id '-continue))))
           (pyapp (Id '-continue) (CSym 'nothing))))))))]

    [else (error 'cps (format "Not handled: ~a" expr))])))

(define (cps-eval expr)
  (local [
    (define result
      (interp-env
            (python-lib
              (CModule '()
              (pyapp (cps expr)
                     ; NOTE(joe): Not todo.  This is the base case of CPS
                     (pylam (V) (Id V))
                     (pylam (V) (CRaise (some (CStr "Top-level return"))))
                     (pylam (V) (CRaise (some (CStr "Top-level exception"))))
                     (pylam (V) (CRaise (some (CStr "Top-level break"))))
                     (pylam (V) (CRaise (some (CStr "Top-level continue")))))))
            (list (hash empty)) (hash empty) empty))
  ]
  (type-case Result result
    [v*s (v s e) v]
    [else (error 'cps-eval (format "Abnormal return: ~a" result))])))

