#lang plai-typed/untyped

(require
  "python-core-syntax.rkt"
  "util.rkt"
  "python-interp.rkt"
  "python-lib.rkt"
  "util.rkt"
  (typed-in racket/base (format : (string 'a -> string)))
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
(define V (gensym 'value-))
(define Vi (Id V))
(define V2 (gensym 'value2-))
(define V2i (Id V2))

(define (cps-list exprs base-thunk)
  (pylam (K R E B C)
    (pyapp Ki (cps-list/help (reverse exprs) empty base-thunk))))

(define (cps-list/help exprs ids base-thunk)
  (cond
    [(empty? exprs) (base-thunk (map Id ids))]
    [(cons? exprs)
     (local [(define id (gensym '-cps/list))]
       (pyapp
        (cps (first exprs))
        (pylam (id) (cps-list/help (rest exprs) (cons id ids) base-thunk))
        Ri Ei Bi Ci))]))


;; NOTE(dbp): the next two were created to deal with dicts, but dicts
;; may be going away, so these could potentially be dropped if that is
;; the case.
(define (cps-hash hsh base-thunk)
  (pylam (K R E B C)
         (pyapp Ki (cps-hash/help (hash-keys hsh) hsh empty base-thunk))))

(define (cps-hash/help keys hsh pairs base-thunk)
  (cond
    [(empty? keys) (base-thunk pairs)]
    [(cons? keys)
     (local [(define key (gensym '-cps/hashkey))
             (define val (gensym '-cps/hashval))]
       (pyapp
        (cps (first keys))
        (pylam (key)
          (pyapp
           (cps (some-v (hash-ref hsh (first keys))))
           (pylam (val) (cps-hash/help (rest keys)
                                       (cons (values (Id key) (Id val)) pairs)
                                       base-thunk))
           Ri Ei Bi Ci))
        Ri Ei Bi Ci))]))


(define (cps expr)
  (local [
    (define (const v)
      (pylam (K R E B C) (pyapp (Id K) v)))
  ]
  (type-case CExpr expr
    ;; NOTE(dbp): yield is a special case, and isn't really getting CPSd at all; instead
    ;; it is glueing together the generator, exploiting what it knows about the environment
    ;; that this CPSd code is in.
    [CYield (expr)
      (pylam (K R E B C)
        (pyapp (cps expr)
          (pylam (V)
            (CSeq (CAssign (CGetField (Id 'self) '___resume)
                           (pylam (V)
                                  (CSeq (CNone) #;(pyapp (gid 'print) (make-builtin-str "s1\n"))
                                        (pyapp Ki Vi))))
                  (CSeq (CNone) #;(pyapp (gid 'print) (make-builtin-str "s2\n"))
                        Vi)))
          Ri Ei Bi Ci))]
    [CSym (s) (const expr)]
    [CTrue () (const expr)]
    [CFalse () (const expr)]
    [CNone () (const expr)]
    [CId (x l) (const expr)]
    [CObject (c b) (const expr)]
    [CFunc (args varargs body opt-class) (const expr)]
    [CUndefined () (const expr)]

    [CGetField (val attr)
      (pylam (K R E B C)
        (pyapp (cps val)
	  (pylam (V)
                 (CSeq (CNone) #;(pyapp (gid 'print) (make-builtin-str "s3\n"))
            (pyapp Ki (CGetField Vi attr))))
	  Ri Ei Bi Ci))]

    [CGetAttr (val attr)
      (pylam (K R E B C)
        (pyapp (cps val)
	  (pylam (V)
            (pyapp (cps attr)
              (pylam (V2)
                     (CSeq (CNone) #;(pyapp (gid 'print) Vi)
                           (CSeq (CNone) #;(pyapp (gid 'print) V2i)
                                 (CSeq
                                  (CNone) #;(pyapp (gid 'print) (CGetAttr Vi V2i))
                (pyapp Ki (CGetAttr Vi V2i))))))
              Ri Ei Bi Ci))
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
	        [none () (CSeq (CNone) #;(pyapp (gid 'print) (make-builtin-str "s5\n"))
                               (CSeq (CNone) #;(pyapp (gid 'print) Vi)
                               (CApp Vi ids (none))))]
		[some (e)
		  (pyapp (cps e)
		    (pylam (V2)
                           (CSeq (CNone) #;(pyapp (gid 'print) (make-builtin-str "s6\n"))
                           (CApp Vi ids (some V2i))))
		  Ri Ei Bi Ci)])))
	   Ki Ei Ri Bi Ci))
	 Ri Ei Bi Ci))]

    [CList (cls values) (cps-list values (lambda (ids) (CList cls ids)))]
    [CTuple (cls values) (cps-list values (lambda (ids) (CTuple cls ids)))]
    [CSet (cls values) (cps-list values (lambda (ids) (CSet cls ids)))]

    [CLet (x typ bind body)
     (pylam (K R E B C)
       (if (CUndefined? bind)
           (CLet x typ bind
                 (pyapp (cps body) Ki Ri Ei Bi Ci))
           (pyapp (cps bind)
                  (pylam (V)
                         (CSeq (CNone) #;(pyapp (gid 'print) (make-builtin-str "s7\n"))
                               (CLet x typ Vi
                                     (pyapp (cps body) Ki Ri Ei Bi Ci))))
                  Ri Ei Bi Ci)))]

    [CBuiltinPrim (op args)
     (cps-list args (lambda (ids) (CBuiltinPrim op ids)))]

    [CIf (test then els)
     (pylam (K R E B C)
       (pyapp (cps test)
        (pylam (V)
               (CSeq (CNone) #;(pyapp (gid 'print) (make-builtin-str "s8\n"))
          (CIf Vi
           (pyapp (cps then) Ki Ri Ei Bi Ci)
           (pyapp (cps els) Ki Ri Ei Bi Ci))))
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
            (pyapp Ki (CSeq (CNone) #;(pyapp (gid 'print) (make-builtin-str "s9\n"))
                                (CAssign (CId x type) Vi))))
           Ri Ei Bi Ci))]
       [CGetField (o a) (error 'cps "Assign to object nyi")]
       [else (error 'cps "CPS: got a non-id, non-obj in CAssign")])]

    [CReturn (val)
     (pylam (K R E B C) (pyapp (cps val) Ri Ri Ei Bi Ci))]

    [CRaise (val)
     (type-case (optionof CVal) val
       [none () (pylam (K R E B C)
                  (pyapp (cps (make-exception
                               '$Reraise
                               "Try to reraise active exception"))
                         Ei Ri Ei Bi Ci))]
       [some (v)
             (pylam (K R E B C) (pyapp (cps v) Ei Ri Ei Bi Ci))])]

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
                      (CSeq (CNone) #;(pyapp (gid 'print) (make-builtin-str "s10\n"))
		(CIf Vi
		  (pyapp (cps body)
                    (pylam (V2) (pyapp (Id '-while) Ki Ri Ei Bi Ci))
                    Ri Ei Bi Ci)
                  (pyapp (Id '-elsethunk)))))
               Ri Ei Bi Ci)))
          (CSeq
           (CAssign (Id '-continue)
            (pylam (V)
	      (pyapp
               (Id '-while)
               Ki Ri Ei Ki ;; NOTE(joe): Break becomes the "normal" continuation Ki
               (Id '-continue))))
           (pyapp (Id '-continue) (CSym 'nothing))))))))]

    [CTryExceptElse
     (try exn excepts els)
     (pylam (K R E B C)
       (pyapp (cps try)
         (pylam (V)
            (pyapp (cps els)
                  Ki Ri Ei Bi Ci))
         Ri
         (pylam (V)
            ;; TODO(dbp): check that this is an exception
                (CSeq (CNone) #;(pyapp (gid 'print) (make-builtin-str "s11\n"))
            (CLet exn (LocalId) Vi
              (pyapp (cps excepts)
                Ki Ri
                (pylam (V2)
                  (CIf
                   (pyapp (gid '%isinstance)
                          V2i
                          (gid '$Reraise))
                   (pyapp Ei Vi)
                   (pyapp Ei V2i)))
                Bi Ci))))
         Bi
         Ci))]

    [CTryFinally
     (try finally)
     (pylam (K R E B C)
       (pyapp (cps try)
         ;; in each case, we run the finally and then put the original
         ;; value back to whatever continuation that it wanted
         (pylam (V)
              (pyapp (cps finally)
                     (pylam (V2)
                       (pyapp Ki Vi))
                     Ri Ei Bi Ci))
         (pylam (V)
              (pyapp (cps finally)
                     (pylam (V2)
                       (pyapp Ri Vi))
                     Ri Ei Bi Ci))
         (pylam (V)
              (pyapp (cps finally)
                     (pylam (V2)
                       (pyapp Ei Vi))
                     Ri Ei Bi Ci))
         (pylam (V)
              (pyapp (cps finally)
                     (pylam (V2)
                       (pyapp Bi Vi))
                     Ri Ei Bi Ci))
         (pylam (V)
              (pyapp (cps finally)
                     (pylam (V2)
                       (pyapp Ci Vi))
                     Ri Ei Bi Ci))))]

    [else (error 'cps (format "Not handled: ~a" expr))])))

(define (cps-eval expr)
  (local [
    (define result
      (begin
        (reset-state)
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
            (list (hash empty)) (hash empty) empty)))
  ]
  (type-case Result result
    [v*s (v s) (type-case CVal v
                 [VPointer (p) (fetch-once p s)]
                 [else v])]
    [else (error 'cps-eval (format "Abnormal return: ~a" result))])))

(define (non-cps-eval expr)
  (local [
    (define result
      (begin
        (reset-state)
        (interp-env
            (python-lib
              (CModule '() expr))
            (list (hash empty)) (hash empty) empty)))
  ]
  (type-case Result result
    [v*s (v s) (type-case CVal v
                 [VPointer (p) (fetch-once p s)]
                 [else v])]
    [else (error 'cps-eval (format "Abnormal return: ~a" result))])))


(define (has-yield? [expr : CExpr])
  (type-case CExpr expr
    [CSeq (e1 e2)
          (or (has-yield? e1)
              (has-yield? e2))]
    [CAssign (target value)
             (or (has-yield? target)
                 (has-yield? value))]
    [CIf (test then els)
         (or (has-yield? test)
             (has-yield? then)
             (has-yield? els))]
    [CLet (x type bind body)
          (or (has-yield? bind) (has-yield? body))]
    [CApp (fun args stararg)
          (or (has-yield? fun)
                (foldr (lambda (a b) (or a b)) false (map has-yield? args))
                (if (some? stararg) (has-yield? (some-v stararg)) false))]
    [CWhile (test body orelse)
            (or (has-yield? test)
                (has-yield? body)
                (has-yield? orelse))]
    [CRaise (expr) (if (some? expr) (has-yield? (some-v expr)) false)]
    [CTryExceptElse (try exn-id excepts orelse)
                    (or (has-yield? try)
                        (has-yield? excepts)
                        (has-yield? orelse))]
    [CTryFinally (try finally) (or (has-yield? try)
                                   (has-yield? finally))]
    [CModule (prelude body) (or (has-yield? prelude)
                                (has-yield? body))]
    [CConstructModule (source)
                      (has-yield? source)]
    [CYield (expr) true]
    ;; functions and primitive values; yields don't go past functions,
    ;; and primitive values can't have them.
    [else false]))

(define (desugar-generators [expr : CExpr])
  (type-case CExpr expr
    ;; Func is the interesting case; if it has a yield, create a generator object.
    [CFunc
     (args varargs body opt-class)
     (if (has-yield? body)
         (let [(kill-generator (CAssign (CGetField (Id 'self) '__next__)
                                                    (CFunc
                                                     (list) (none)
                                                     (CRaise (some (make-exception
                                                                    'StopIteration
                                                                    "generator terminated")))
                                                     ;; NOTE(dbp): is this the right symbol?
                                                     (some '%generator))))]
         (CFunc
          args
          varargs
          (CReturn (pyapp (gid '%generator)
                           ;; NOTE(dbp): we pass in an initializing function and
                           ;; a __next__ function.
                           (CFunc (list 'self) (none)
                                  (CAssign (CGetField (Id 'self) '___resume)
                                          (CFunc (list 'arg) (none)
                                           (CReturn (pyapp
                                           (cps body)
                                           ;; NOTE(dbp):
                                           ;; if we ever end, this is a StopIteration exception,
                                           ;; and we kill the generator
                                           (pylam
                                            (V)
                                            (CSeq
                                             kill-generator
                                             (CRaise (some (make-exception 'StopIteration
                                                                           "generator terminated")))))
                                          ;; NOTE(dbp): return inside generators is illegal...
                                          ;; so in a sense our even handling this case is a little
                                          ;; odd...
                                         (pylam
                                          (V)
                                          (CSeq
                                           kill-generator
                                          (CRaise (some
                                                   (make-exception 'SyntaxError
                                                                   "return in generator - bad!")))))
                                         ;; NOTE(dbp):
                                         ;; an uncaught exception within the generator propogates
                                         ;; it and kills the generator (see gen-exception.py test)
                                         (pylam
                                          (V)
                                          (CSeq
                                           kill-generator
                                           (CRaise (some Vi))))
                                         ;; break and continue are illegal (SyntaxErrors).
                                         ;; they should not get here; if they do, we are
                                         ;; seriously hosed. For now, just kill the generator,
                                         ;; but really, more serious measures should be made.
                                         (pylam
                                          (V)
                                          (CSeq
                                           kill-generator
                                          (CRaise (some
                                                   (make-exception 'SyntaxError
                                                                   "break outside of loop - bad!")))))
                                         (pylam
                                          (V)
                                          (CRaise (some
                                                   (make-exception 'SyntaxError
                                                                   "continue outside of loop - bad!"))))))
                                           (none)))
                                  (none))))
         (none)))
         (CFunc args varargs (desugar-generators body) opt-class))]
    ;; All the rest is pure recursion
    [CSeq (e1 e2)
          (CSeq (desugar-generators e1)
                (desugar-generators e2))]
    [CAssign (target value)
             (CAssign (desugar-generators target)
                      (desugar-generators value))]
    [CIf (test then els)
         (CIf (desugar-generators test)
              (desugar-generators then)
              (desugar-generators els))]
    [CLet (x type bind body)
          (CLet x type (desugar-generators bind) (desugar-generators body))]
    [CApp (fun args stararg)
          (CApp (desugar-generators fun)
                (map desugar-generators args)
                (if (some? stararg) (some (desugar-generators (some-v stararg))) stararg))]
    [CWhile (test body orelse)
            (CWhile (desugar-generators test)
                    (desugar-generators body)
                    (desugar-generators orelse))]
    [CRaise (expr) (if (some? expr)
                       (CRaise (some (desugar-generators (some-v expr))))
                       (CRaise expr))]
    [CTryExceptElse (try exn-id excepts orelse)
                    (CTryExceptElse (desugar-generators try)
                                    exn-id
                                    (desugar-generators excepts)
                                    (desugar-generators orelse))]
    [CTryFinally (try finally) (CTryFinally (desugar-generators try)
                                            (desugar-generators finally))]
    [CModule (prelude body) (CModule (desugar-generators prelude)
                                     (desugar-generators body))]
    [CConstructModule (source)
                      (CConstructModule (desugar-generators source))]
    [CYield (expr) (error 'desugar-generators "Should not have been able to get a CYield")]
    [else expr]))
