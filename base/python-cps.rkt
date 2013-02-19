#lang plai-typed/untyped

(require
  "python-core-syntax.rkt"
  "python-interp.rkt"
  (typed-in racket/base (format : (string 'a -> string)))
  (typed-in racket/base (gensym : (symbol -> symbol))))

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

    [CRaise (val)
     (pylam (K R E B C)
       (pyapp (cps val) Ei Ri Ei Bi Ci))]

    [else (error 'cps (format "Not handled: ~a" expr))])))

(define (cps-eval expr)
  (v*s-v
    (interp-env
      (pyapp (cps expr)
             ; NOTE(joe): Not todo.  This is the base case of CPS
             (pylam (V) (Id V))
             (pylam (V) (CRaise (some (CStr "Top-level return"))))
             (pylam (V) (CRaise (some (CStr "Top-level exception"))))
             (pylam (V) (CRaise (some (CStr "Top-level break"))))
             (pylam (V) (CRaise (some (CStr "Top-level continue")))))
      (list (hash empty)) (hash empty) empty)))

