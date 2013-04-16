#lang plai-typed/untyped

(require
  "python-core-syntax.rkt"
  (typed-in racket/base (hash-map : ((hashof 'a 'b) ('a 'b -> c) -> (listof 'c)))))

(define (var->sexp/opt maybe-var)
  (type-case (optionof symbol) maybe-var
    [some (x) `(some-var ,x)]
    [none () '(no-var)]))

(define (env->sexp env)
  `(env ,(hash-map env (lambda (k v) (cons k v)))))
(define (store->sexp sto)
  (type-case Store sto
    [store (locs next-loc)
      `(store ,(hash-map locs (lambda (k v) (cons k (val->sexp v)))) ,next-loc)]))

(define (mval->sexp mval)
  (local [
    (define (mdict->sexp d)
      (hash-map d (lambda (k v) (cons (val->sexp k) (val->sexp v)))))
  ]
  (type-case MetaVal mval
    [MetaNone () `(meta-none)]
    [MetaNum (n) `(meta-num ,n)]
    [MetaStr (s) `(meta-str ,s)]
    [MetaClass (c) `(meta-class ,c)]
    [MetaList (vs) `(meta-list ,(map val->sexp vs))]
    [MetaTuple (vs) `(meta-tuple ,(map val->sexp vs))]
    [MetaSet (vs) `(meta-set ,(map val->sexp vs))]
    [MetaDict (d) `(meta-dict ,(mdict->sexp d))]
    [MetaClosure (e xs x b c)
     `(meta-closure ,(env->sexp e)
                    ,xs
                    ,(var->sexp/opt x)
                    ,(core->sexp b)
                    ,(var->sexp/opt c))]
    [MetaCode (e f gs)
     `(meta-code ,(core->sexp e) ,f ,gs)]
    [MetaPort (p) (error 'core->sexp "Cannot serialize port value")])))
(define (mval->sexp/opt maybe-mval)
  (type-case (optionof MetaVal) maybe-mval
    [some (m) `(some-meta ,(mval->sexp m))]
    [none () '(no-meta)]))

(define (val->sexp/opt maybe-val)
  (type-case (optionof CVal) maybe-val
    [some (v) `(some-val ,(val->sexp v))]
    [none () `(no-val)]))

(define (val->sexp value)
  (local [
    (define (dict->sexp d)
      (hash-map d (lambda (k a) (cons k a))))
  ]
  (type-case CVal value
    [VObjectClass (ante mval dict class)
     `(vobjectclass ,ante
                    ,(mval->sexp/opt mval)
                    ,(dict->sexp dict)
                    ,(val->sexp/opt class))]
    [VUndefined () '(vundefined)]
    [VSym (s) `(vsym ,s)]
    [VPointer (a) `(vpointer ,a)])))

(define (core->sexp core-stx)
  (local [
    (define (idtype->sexp idtype)
      (type-case IdType idtype
        [LocalId () 'local]
        [GlobalId () 'global]))
    (define (core->sexp/opt val)
      (type-case (optionof CExpr) val
        [some (e) `(some-e ,(core->sexp e))]
        [none () '(no-e)]))
  ]
  (type-case CExpr core-stx
    [CSym (s) `(sym ,s)]
    [CTrue () `(true)]
    [CFalse () `(false)]
    [CNone () `(none)]
    [CObject (class mval)
     `(object ,(core->sexp class) ,(mval->sexp/opt mval))]
    [CGetAttr (value attr)
     `(get-attr ,(core->sexp value) ,(core->sexp attr))]
    [CSetAttr (obj attr value)
     `(set-attr ,(core->sexp obj) ,(core->sexp attr) ,(core->sexp value))]
    [CId (x type)
     `(id ,x ,(idtype->sexp type))]
    [CSeq (e1 e2) `(seq ,(core->sexp e1) ,(core->sexp e2))]
    [CAssign (target value)
     `(assign ,(core->sexp target) ,(core->sexp value))]
    [CIf (test then els)
     `(if ,(core->sexp test) ,(core->sexp then) ,(core->sexp els))]
    [CLet (x type bind body)
     `(let ,x ,(idtype->sexp type) ,(core->sexp bind) ,(core->sexp body))]
    [CBuiltinPrim (op args)
     `(builtin-prim ,op
                    ,(map core->sexp args))]
    [CApp (fun args stararg)
     `(app ,(core->sexp fun)
           ,(map core->sexp args)
           ,(core->sexp/opt stararg))]
    [CFunc (args varargs body opt-class)
     `(fun ,args
                ,(var->sexp/opt varargs)
                ,(core->sexp body)
                ,(var->sexp/opt opt-class))]
    [CWhile (test body orelse)
     `(while ,(core->sexp test)
                  ,(core->sexp body)
                  ,(core->sexp orelse))]
    [CReturn (value) `(return ,(core->sexp value))]
    [CList (class values)
     `(list ,(core->sexp class) ,(map core->sexp values))]
    [CTuple (class values)
     `(tuple ,(core->sexp class) ,(map core->sexp values))]
    [CSet (class values)
     `(set ,(core->sexp class) ,(map core->sexp values))]
    [CRaise (expr)
     `(raise ,(core->sexp/opt expr))]
    [CTryExceptElse (try exn-id excepts orelse)
     `(tryexcept ,(core->sexp try) ,exn-id
                      ,(core->sexp excepts)
                      ,(core->sexp orelse))]
    [CTryFinally (try finally)
     `(tryfinally ,(core->sexp try)
                       ,(core->sexp finally))]
    [CBreak () `(break)]
    [CContinue () `(continue)]
    [CUndefined () `(undefined)]
    [CModule (prelude body)
     `(module ,(core->sexp prelude) ,(core->sexp body))]
    [CConstructModule (source)
     `(construct-module ,(core->sexp source))]
    [CYield (e) `(yield ,(core->sexp e))])))

