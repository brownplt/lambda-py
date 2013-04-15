#lang racket/base

(require
  racket/match
  (only-in plai-typed some none)
  "python-core-syntax.rkt")

(provide sexp->core sexp->store sexp->env sexp->val)

(define (sexp->env sexp)
  (match sexp
    [(list 'env (? list? pairs))
     (make-immutable-hash pairs)]
    [_ (error 'sexp->env (format "Bad env: ~a" sexp))]))
(define (sexp->store sexp)
  (match sexp
    [(list 'store (? list? locs) next-loc)
     (define locs-vals
      (make-immutable-hash
        (map (lambda (p) (cons (car p) (sexp->val (cdr p))))
             locs)))
     (store locs-vals next-loc)]
    [_ (error 'sexp->store (format "Bad store: ~a" sexp))]))

(define (sexp->mval mval)
  (define (sexp->mdict sexp)
    (match sexp
      [(? list? pairs)
       (make-immutable-hash
        (map (lambda (p) (cons (sexp->val (car p))
                               (sexp->val (cdr p))))
             pairs))]
      [_ (error 'sexp->mdict (format "Weird mdict: ~a" sexp))]))
  (match mval
    [(list 'meta-none) (MetaNone)]
    [(list 'meta-num n) (MetaNum n)]
    [(list 'meta-str s) (MetaStr s)]
    [(list 'meta-class c) (MetaClass c)]
    [(list 'meta-list vs) (MetaList (map sexp->val vs))]
    [(list 'meta-tuple vs) (MetaTuple (map sexp->val vs))]
    [(list 'meta-set vs) (MetaSet (map sexp->val vs))]
    [(list 'meta-dict d) (MetaDict (sexp->mdict d))]
    [(list 'meta-closure e xs x b c)
     (MetaClosure (sexp->env e)
                  xs
                  (sexp->var/opt x)
                  (sexp->core b)
                  (sexp->var/opt c))]
    [(list 'meta-code e f g)
     (MetaCode (sexp->core e) f g)]
    [else (error 'sexp->core (format "Bizarre mval sexp: ~a" mval))]))
(define (sexp->mval/opt maybe-mval)
  (match maybe-mval
    [(list 'some-meta m) (some (sexp->mval m))]
    [(list 'no-meta) (none)]
    [else (error 'sexp->core (format "Bizarre mval/opt sexp: ~a" maybe-mval))]))
(define (sexp->var/opt maybe-var)
  (match maybe-var
    [(list 'some-var x) (some x)]
    [(list 'no-var) (none)]))

(define (sexp->val/opt maybe-val)
  (match maybe-val
    [(list 'some-val v) (some (sexp->val v))]
    [(list 'no-val) (none)]))
(define (sexp->val sexp)
  (define (sexp->dict sexp)
    (match sexp
      [(? list? pairs)
       (make-immutable-hash pairs)]
      [_ (error 'sexp->dict (format "Weird dict: ~a" sexp))]))
  (match sexp
    [(list 'vobjectclass ante mval dict class)
     (VObjectClass ante
                   (sexp->mval/opt mval)
                   (sexp->dict dict)
                   (sexp->val/opt class))]
    [(list 'vundefined)
     (VUndefined)]
    [(list 'vsym s)
     (VSym s)]
    [(list 'vpointer a)
     (VPointer a)]))
  

(define (sexp->core sexp)
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
