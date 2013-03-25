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
    (pyapp-simple Ki (cps-list/help (reverse exprs) empty base-thunk))))

(define (cps-list/help exprs ids base-thunk)
  (cond
    [(empty? exprs) (base-thunk (map Id ids))]
    [(cons? exprs)
     (local [(define id (gensym '-cps/list))]
       (pyapp-simple
        (cps (first exprs))
        (pylam (id) (cps-list/help (rest exprs) (cons id ids) base-thunk))
        Ri Ei Bi Ci))]))


;; NOTE(dbp): the next two were created to deal with dicts, but dicts
;; may be going away, so these could potentially be dropped if that is
;; the case.
(define (cps-hash hsh base-thunk)
  (pylam (K R E B C)
         (pyapp-simple Ki (cps-hash/help (hash-keys hsh) hsh empty base-thunk))))

(define (cps-hash/help keys hsh pairs base-thunk)
  (cond
    [(empty? keys) (base-thunk pairs)]
    [(cons? keys)
     (local [(define key (gensym '-cps/hashkey))
             (define val (gensym '-cps/hashval))]
       (pyapp-simple
        (cps (first keys))
        (pylam (key)
          (pyapp-simple
           (cps (some-v (hash-ref hsh (first keys))))
           (pylam (val) (cps-hash/help (rest keys)
                                       (cons (values (Id key) (Id val)) pairs)
                                       base-thunk))
           Ri Ei Bi Ci))
        Ri Ei Bi Ci))]))


(define (cps expr)
  (local [
    (define (const v)
      (pylam (K R E B C) (pyapp-simple (Id K) v)))
  ]
  (type-case CExpr expr
    ;; NOTE(dbp): yield is a special case, and isn't really getting CPSd at all; instead
    ;; it is glueing together the generator, exploiting what it knows about the environment
    ;; that this CPSd code is in.
    [CYield (expr)
      (pylam (K R E B C)
        (pyapp-simple (cps expr)
          (pylam (V)
            (CSeq (set-field (Id 'self) '___resume
                             (pylam (V)
                                    (CSeq (CNone) #;(pyapp-simple (gid 'print) (make-builtin-str "s1\n"))
                                          (pyapp-simple Ki Vi))))
                  (CSeq (CNone) #;(pyapp-simple (gid 'print) (make-builtin-str "s2\n"))
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

    [CGetAttr (val attr)
      (pylam (K R E B C)
        (pyapp-simple (cps val)
	  (pylam (V)
            (pyapp-simple (cps attr)
              (pylam (V2)
                     (CSeq (CNone) #;(pyapp-simple (gid 'print) Vi)
                           (CSeq (CNone) #;(pyapp-simple (gid 'print) V2i)
                                 (CSeq
                                  (CNone) #;(pyapp-simple (gid 'print) (CGetAttr Vi V2i))
                (pyapp-simple Ki (CGetAttr Vi V2i))))))
              Ri Ei Bi Ci))
          Ri Ei Bi Ci))]
    
    [CApp (fun args stararg)
     (pylam (K R E B C)
       (pyapp-simple (cps fun)
         (pylam (V)
          (pyapp-simple
	   (cps-list
	    args 
	    (lambda (ids)
	      (type-case (optionof CExpr) stararg
	        [none () (CSeq (CNone) #;(pyapp-simple (gid 'print) (make-builtin-str "s5\n"))
                               (CSeq (CNone) #;(pyapp-simple (gid 'print) Vi)
                               (py-app Vi ids (none))))]
		[some (e)
		  (pyapp-simple (cps e)
		    (pylam (V2)
                           (CSeq (CNone) #;(pyapp-simple (gid 'print) (make-builtin-str "s6\n"))
                           (py-app Vi ids (some V2i))))
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
                 (pyapp-simple (cps body) Ki Ri Ei Bi Ci))
           (pyapp-simple (cps bind)
                  (pylam (V)
                         (CSeq (CNone) #;(pyapp-simple (gid 'print) (make-builtin-str "s7\n"))
                               (CLet x typ Vi
                                     (pyapp-simple (cps body) Ki Ri Ei Bi Ci))))
                  Ri Ei Bi Ci)))]

    [CBuiltinPrim (op args)
     (cps-list args (lambda (ids) (CBuiltinPrim op ids)))]

    [CIf (test then els)
     (pylam (K R E B C)
       (pyapp-simple (cps test)
        (pylam (V)
               (CSeq (CNone) #;(pyapp-simple (gid 'print) (make-builtin-str "s8\n"))
          (CIf Vi
           (pyapp-simple (cps then) Ki Ri Ei Bi Ci)
           (pyapp-simple (cps els) Ki Ri Ei Bi Ci))))
        Ri Ei Bi Ci))]

    [CSeq (e1 e2)
     (pylam (K R E B C)
       (pyapp-simple
        (cps e1)
        (pylam (V) (pyapp-simple (cps e2) Ki Ri Ei Bi Ci))
        Ri Ei Bi Ci))]

    [CAssign (lhs rhs)
     (type-case CExpr lhs
       [CId (x type)
        (pylam (K R E B C)
          (pyapp-simple
           (cps rhs)
           (pylam (V)
            (pyapp-simple Ki (CSeq (CNone) #;(pyapp-simple (gid 'print) (make-builtin-str "s9\n"))
                                (CAssign (CId x type) Vi))))
           Ri Ei Bi Ci))]
       [else (error 'cps "CPS: got a non-id in CAssign")])]

    [CReturn (val)
     (pylam (K R E B C) (pyapp-simple (cps val) Ri Ri Ei Bi Ci))]

    [CRaise (val)
     (type-case (optionof CVal) val
       [none () (pylam (K R E B C)
                  (pyapp-simple (cps (make-exception
                               '$Reraise
                               "Try to reraise active exception"))
                         Ei Ri Ei Bi Ci))]
       [some (v)
             (pylam (K R E B C) (pyapp-simple (cps v) Ei Ri Ei Bi Ci))])]

    [CBreak ()
     (pylam (K R E B C) (pyapp-simple Bi (CNone)))]

    [CContinue ()
     (pylam (K R E B C) (pyapp-simple Ci (CNone)))]

    [CWhile (test body els)
     (pylam (K R E B C)
       (Let '-while (CSym 'nothing)
       (Let '-continue (CSym 'nothing)
       (Let '-elsethunk
         (pylam ()
           (pyapp-simple (cps els) Ki Ri Ei Bi Ci))
         (CSeq
          (CAssign (Id '-while)
            (pylam (K R E B C)
              (pyapp-simple (cps test)
               (pylam (V)
                      (CSeq (CNone) #;(pyapp-simple (gid 'print) (make-builtin-str "s10\n"))
		(CIf Vi
		  (pyapp-simple (cps body)
                    (pylam (V2) (pyapp-simple (Id '-while) Ki Ri Ei Bi Ci))
                    Ri Ei Bi Ci)
                  (pyapp-simple (Id '-elsethunk)))))
               Ri Ei Bi Ci)))
          (CSeq
           (CAssign (Id '-continue)
            (pylam (V)
	      (pyapp-simple
               (Id '-while)
               Ki Ri Ei Ki ;; NOTE(joe): Break becomes the "normal" continuation Ki
               (Id '-continue))))
           (pyapp-simple (Id '-continue) (CSym 'nothing))))))))]

    [CTryExceptElse
     (try exn excepts els)
     (pylam (K R E B C)
       (pyapp-simple (cps try)
         (pylam (V)
            (pyapp-simple (cps els)
                  Ki Ri Ei Bi Ci))
         Ri
         (pylam (V)
            ;; TODO(dbp): check that this is an exception
                (CSeq (CNone) #;(pyapp-simple (gid 'print) (make-builtin-str "s11\n"))
            (CLet exn (LocalId) Vi
              (pyapp-simple (cps excepts)
                Ki Ri
                (pylam (V2)
                  (CIf
                   (pyapp-simple (gid '%isinstance)
                          V2i
                          (gid '$Reraise))
                   (pyapp-simple Ei Vi)
                   (pyapp-simple Ei V2i)))
                Bi Ci))))
         Bi
         Ci))]

    [CTryFinally
     (try finally)
     (pylam (K R E B C)
       (pyapp-simple (cps try)
         ;; in each case, we run the finally and then put the original
         ;; value back to whatever continuation that it wanted
         (pylam (V)
              (pyapp-simple (cps finally)
                     (pylam (V2)
                       (pyapp-simple Ki Vi))
                     Ri Ei Bi Ci))
         (pylam (V)
              (pyapp-simple (cps finally)
                     (pylam (V2)
                       (pyapp-simple Ri Vi))
                     Ri Ei Bi Ci))
         (pylam (V)
              (pyapp-simple (cps finally)
                     (pylam (V2)
                       (pyapp-simple Ei Vi))
                     Ri Ei Bi Ci))
         (pylam (V)
              (pyapp-simple (cps finally)
                     (pylam (V2)
                       (pyapp-simple Bi Vi))
                     Ri Ei Bi Ci))
         (pylam (V)
              (pyapp-simple (cps finally)
                     (pylam (V2)
                       (pyapp-simple Ci Vi))
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
              (pyapp-simple (cps expr)
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
         (let [(kill-generator (set-field (Id 'self) '___resume
                                          (CFunc
                                           (list 'arg) (none)
                                           (CRaise (some (make-exception
                                                          'StopIteration
                                                          "generator terminated")))
                                           ;; NOTE(dbp): is this the right symbol?
                                           (none))))]
         (CFunc
          args
          varargs
          (CReturn (pyapp (gid '%generator)
                           ;; NOTE(dbp): we pass in an initializing function and
                           ;; a __next__ function.
                           (CFunc (list 'self) (none)
                                  (set-field (Id 'self) '___resume
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
                                            ;; NOTE(dbp): return inside generator kills the generator
                                           (pylam
                                            (V)
                                            (CSeq
                                             kill-generator
                                            (CRaise (some
                                                     (make-exception 'StopIteration
                                                                     "generator terminated")))))
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
