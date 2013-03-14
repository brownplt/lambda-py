#lang plai-typed/untyped

(require
  "python-core-syntax.rkt"
  "util.rkt"
  "python-interp.rkt"
  "python-lib.rkt"
  "util.rkt"
  (typed-in racket/base (format : (string 'a -> string)))
  (typed-in racket/base (gensym : (symbol -> symbol))))

(define (pyget val fld)
  (pyapp (CGetField val '__getitem__)
	 fld))

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
(define V2i (Id V2))

(define (cps-list exprs base-thunk)
  (pylam (K R E B C)
    (cps-list/help exprs empty base-thunk)))

(define (cps-list/help exprs ids base-thunk)
  (cond
    [(empty? exprs) (base-thunk (map Id ids))]
    [(cons? exprs)
     (local [(define id (gensym '-cps/list))]
       (pyapp
        (cps (first exprs))
        (pylam (id) (cps-list/help (rest exprs) (cons id ids) base-thunk))
        Ri Ei Bi Ci))]))

(define (cps expr)
  (local [
    (define (const v)
      (pylam (K R E B C) (pyapp (Id K) v)))
  ]
  (type-case CExpr expr
    [CSym (s) (const expr)]
    [CTrue () (const expr)]
    [CFalse () (const expr)]
    [CNone () (const expr)]
    [CId (x l) (const expr)]
    [CObject (c b) (const expr)]
    [CFunc (args varargs body opt-class) (const expr)]

    [CClass (nm bases body) (error 'cps "Not written yet")]
    [CGetField (val attr)
      (pylam (K R E B C)
	(pyapp (cps val)
	  (pylam (V)
	    (CGetField Vi attr))
	  Ri Ei Bi Ci))]
    
    [CApp (fun args stararg)
     (pylam (K R E B C)
       (pyapp (cps fun)
         (pylam (V)
          (pyapp 
	   (cps-list
	    args 
	    (lambda (ids)
	      (type-case (optionof CExpr) stararg
	        [none () (CApp Vi ids (none))]
		[some (e)
		  (pyapp (cps e)
		    (pylam (V2)
		    (CApp Vi ids (some V2i)))
		  Ri Ei Bi Ci)])))
	   Ki Ei Ri Bi Ci))
	 Ri Ei Bi Ci))]

    [CList (cls values) (error 'cps "Not written yet")]
    [CTuple (cls values) (cps-list values (lambda (ids) (CTuple cls ids)))]
    [CDict (cls contents) (error 'cps "Not written yet")]
    [CSet (cls values) (error 'cps "Not written yet")]

    [CLet (x typ bind body)
     (pylam (K R E B C)
       (pyapp (cps bind)
        (pylam (V)
          (CLet x typ Vi
            (pyapp (cps body) Ki Ri Ei Bi Ci)))
        Ri Ei Bi Ci))]

    [CPrim1 (op e1)
     (pylam (K R E B C)
       (pyapp (cps e1)
        (pylam (V)
          (pyapp Ki (CPrim1 op Vi)))))] 

    [CPrim2 (op e1 e2)
     (pylam (K R E B C)
       (pyapp (cps e1)
        (pylam (V)
          (pyapp (cps e2)
           (pylam (V2)
            (pyapp Ki (CPrim2 op Vi V2i)))
           Ri Ei Bi Ci))
        Ri Ei Bi Ci))]

    [CBuiltinPrim (op args)
     (cps-list args (lambda (ids) (CBuiltinPrim op ids)))]

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
		(CSeq
		 (pyapp (gid 'print) (CSym 'test-continuation))
                (CIf Vi
		  (CSeq
		   (pyapp (gid 'print) (CSym 'body-of-while))
                  (pyapp (cps body)
                    (pylam (V2) (pyapp (Id '-while) Ki Ri Ei Bi Ci))
                    Ri Ei Bi Ci))
                  (pyapp (Id '-elsethunk)))))
               Ri Ei Bi Ci)))
          (CSeq
           (CAssign (Id '-continue)
            (pylam (V)
	      (CSeq
	       (pyapp (gid 'print) (CSym 'continue-continuation))
              (pyapp
               (Id '-while)
               Ki Ri Ei Ki ;; NOTE(joe): Break becomes the "normal" continuation Ki
               (Id '-continue)))))
           (pyapp (Id '-continue) (CSym 'nothing))))))))]

    #;[CTryExceptElseFinally (try excepts orelse finally) (error 'cps "Not written yet")]


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
                     (pylam (V) (CRaise (some (CSym 'Top-level-return))))
                     (pylam (V) (CRaise (some (CSym 'Top-level-exception))))
                     (pylam (V) (CRaise (some (CSym 'Top-level-break))))
                     (pylam (V) (CRaise (some (CSym 'Top-level-continue)))))))
            (list (hash empty)) (hash empty) empty))
  ]
  (type-case Result result
    [v*s (v s) v]
    [else (error 'cps-eval (format "Abnormal return: ~a" result))])))
