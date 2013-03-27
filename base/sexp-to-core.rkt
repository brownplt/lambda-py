#lang racket/base

(require
  racket/match
  (only-in plai-typed some none)
  "python-core-syntax.rkt")

(provide sexp->core)

(define (sexp->core sexp)
  (define (sexp->var/opt maybe-var)
    (match maybe-var
      [(list 'some-var x) (some x)]
      [(list 'no-var) (none)]))
  (define (sexp->mval mval)
    (match mval
      [(list 'meta-none) (MetaNone)]
      [(list 'meta-num n) (MetaNum n)]
      [(list 'meta-str s) (MetaStr s)]
      [(list 'meta-class c) (MetaClass c)]
      [else (error 'sexp->core (format "Bizarre mval sexp: ~a" mval))]))
  (define (sexp->mval/opt maybe-mval)
    (match maybe-mval
      [(list 'some-meta m) (some (sexp->mval m))]
      [(list 'no-meta) (none)]
      [else (error 'sexp->core (format "Bizarre mval/opt sexp: ~a" maybe-mval))]))
  (define (sexp->idtype idtype)
    (match idtype
      ['local (LocalId)]
      ['global (GlobalId)]))
  (define (sexp->core/opt val)
    (match val
      [(list 'some-e e) (some (sexp->core e))]
      [(list 'no-e) (none)]))
  (define sc sexp->core)
  (match sexp
    [(list 'sym s) (CSym s)]
    [(list 'true) (CTrue)]
    [(list 'false) (CFalse)]
    [(list 'none) (CNone)]
    [(list 'object cls mval)
     (CObject (sc cls) (sexp->mval/opt mval))]
    [(list 'get-attr obj attr)
     (CGetAttr (sc obj) (sc attr))]
    [(list 'set-attr obj attr value)
     (CSetAttr (sc obj) (sc attr) (sc value))]
    [(list 'id id type)
     (CId id (sexp->idtype type))]
    [(list 'seq e1 e2)
     (CSeq (sc e1) (sc e2))]
    [(list 'assign target val)
     (CAssign (sc target) (sc val))]
    [(list 'if test thn els)
     (CIf (sc test) (sc thn) (sc els))]
    [(list 'let x type bind body)
     (CLet x (sexp->idtype type) (sc bind) (sc body))]
    [(list 'builtin-prim op args)
     (CBuiltinPrim op (map sc args))]
    [(list 'app fun args star)
     (CApp (sc fun) (map sc args) (sexp->core/opt star))]
    [(list 'fun args var body cls)
     (CFunc args (sexp->var/opt var) (sc body) (sexp->var/opt cls))]
    [(list 'while test body orelse)
     (CWhile (sc test) (sc body) (sc orelse))]
    [(list 'return expr)
     (CReturn (sc expr))]
    [(list 'list cls vals)
     (CList (sc cls) (map sc vals))]
    [(list 'tuple cls vals)
     (CTuple (sc cls) (map sc vals))]
    [(list 'set cls vals)
     (CSet (sc cls) (map sc vals))]
    [(list 'raise expr)
     (CRaise (sexp->core/opt expr))]
    [(list 'tryexcept try id except els)
     (CTryExceptElse (sc try) id (sc except) (sc els))]
    [(list 'tryfinally try finally)
     (CTryFinally (sc try) (sc finally))]
    [(list 'break) (CBreak)]
    [(list 'continue) (CContinue)]
    [(list 'undefined) (CUndefined)]
    [(list 'module e1 e2)
     (CModule (sc e1) (sc e2))]
    [(list 'construct-module e)
     (CConstructModule (sc e))]
    [(list 'yield e)
     (CYield (sc e))]))
