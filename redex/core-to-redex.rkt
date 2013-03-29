#lang plai-typed/untyped

(require
  (typed-in racket/list (take : ((listof 'a) number -> (listof 'a))))
  (typed-in racket/list (last : ((listof 'a) -> 'a)))
  redex
  "../base/python-core-syntax.rkt"
  "lambdapy-core.rkt")

(define (core->redex core-stx)
  (local [
    (define (var->redex/opt maybe-var)
      (type-case (optionof symbol) maybe-var
        [some (x) (term (,x))]
        [none () (term (no-var))]))
    (define (mval->redex mval)
      (type-case MetaVal mval
        [MetaNone () (term (meta-none))]
        [MetaNum (n) (term (meta-num ,n))]
        [MetaStr (s) (term (meta-str ,s))]
        [MetaClass (c) (term (meta-class ,c))]
        [else (error 'core->redex (string-append "Shouldn't be seeing this mval in surface output: " (to-string mval)))]))
    (define (mval->redex/opt maybe-mval)
      (type-case (optionof MetaVal) maybe-mval
        [some (m) (mval->redex m)]
        [none () (term (no-meta))]))
    (define (idtype->redex idtype)
      (type-case IdType idtype
        [LocalId () 'local]
        [GlobalId () 'global]))
    (define (core->redex/opt val)
      (type-case (optionof CExpr) val
        [some (e) (core->redex e)]
        [none () (term ())]))
  ]
  (type-case CExpr core-stx
    [CSym (s) (term (sym ,(symbol->string s)))]
    [CTrue () (term (alloc (obj-val %bool (meta-num 1) ())))]
    [CFalse () (term (alloc (obj-val %bool (meta-num 0) ())))]
    [CNone () (term (alloc (obj-val %none (meta-none) ())))]
    [CObject (class mval)
     (term (object ,(core->redex class) ,(mval->redex/opt mval)))]
    [CGetAttr (value attr)
     (term (get-attr ,(core->redex value) ,(core->redex attr)))]
    [CSetAttr (obj attr value)
     (term (set-attr ,(core->redex obj) ,(core->redex attr) := ,(core->redex value)))]
    [CId (x type)
     (term (id ,x ,(idtype->redex type)))]
    [CSeq (e1 e2) (term (seq ,(core->redex e1) ,(core->redex e2)))]
    [CAssign (target value)
     (term (assign ,(core->redex target) := ,(core->redex value)))]
    [CIf (test then els)
     (term (if ,(core->redex test) ,(core->redex then) ,(core->redex els)))]
    [CLet (x type bind body)
     (let [(result (term (let (,x ,(idtype->redex type) = ,(core->redex bind)) in
      ,(core->redex body))))]
      (begin
        #;(display (string-append "Result of let: " (string-append (to-string (length result)) "\n\n")))
        result))]
    [CApp (fun args stararg)
     (type-case (optionof CExpr) stararg
      [some (e)
       (term (app ,(core->redex fun)
                  ,(map core->redex args)
                  ,(core->redex e)))]
      [none ()
       (term (app ,(core->redex fun)
                  ,(map core->redex args)))])]
    ;; NOTE(joe): We explicitly drop the opt-class, so we don't
    ;; quite capture no-argument super() in the core
    [CFunc (args varargs body opt-class)
     (term (fun ,args
                ,(var->redex/opt varargs)
                ,(core->redex body)))]
    [CWhile (test body orelse)
     (term (while ,(core->redex test)
                  ,(core->redex body)
                  ,(core->redex orelse)))]
    [CReturn (value) (term (return ,(core->redex value)))]
    [CBuiltinPrim (op args)
     (prim->redex op args)]
    [CList (class values)
     (term (list ,(core->redex class) ,(map core->redex values)))]
    [CTuple (class values)
     (term (tuple ,(core->redex class) ,(map core->redex values)))]
    [CSet (class values)
     (term (set ,(core->redex class) ,(map core->redex values)))]
    [CRaise (expr)
     (term (raise ,(core->redex/opt expr)))]
    [CTryExceptElse (try exn-id excepts orelse)
     (term (tryexcept ,(core->redex try) ,exn-id
                      ,(core->redex excepts)
                      ,(core->redex orelse)))]
    [CTryFinally (try finally)
     (term (tryfinally ,(core->redex try)
                       ,(core->redex finally)))]
    [CBreak () (term break)]
    [CContinue () (term continue)]
    [CUndefined () (term (undefined-val))]
    [CModule (prelude body)
     (term (module ,(core->redex prelude) ,(core->redex body)))]
    [CConstructModule (source)
     (term (construct-module ,(core->redex source)))]
    [CYield (e) (error 'core-to-redex "The Redex model doesn't know anything about CYield.  Use CPS to remove CYields before calling core-to-redex")])))

(define (prim-noalloc op args)
  (term (builtin-prim ,op ,args)))
(define (prim-alloc op args)
  (term (alloc (builtin-prim ,op ,args))))
(define (prim-update op to-update args)
  (term (set! ,to-update (builtin-prim ,op ,args))))
(define (fetch-heads l1 l2)
  (append (take l1 (- (length l1) 1)) (list (last l2))))

(define (prim->redex opsym args)
  (local [
    (define argvs (map (lambda (a) (term (fetch ,(core->redex a)))) args))
    (define argsptrs (map (lambda (a) (core->redex a)) args))
    (define op (symbol->string opsym))
  ]
  (case opsym
    ['num+ (prim-alloc op argvs)]
    ['num- (prim-alloc op argvs)]
    ['num* (prim-alloc op argvs)]
    ['num/ (prim-alloc op argvs)]
    ['num// (prim-alloc op argvs)]
    ['num% (prim-alloc op argvs)]
    ['num= (prim-noalloc op argvs)]
    ['num> (prim-noalloc op argvs)]
    ['num< (prim-noalloc op argvs)]
    ['num>= (prim-noalloc op argvs)]
    ['num<= (prim-noalloc op argvs)]
    ['numcmp (prim-alloc op argvs)]
    ['num-str (prim-alloc op argvs)]

    ['str (prim-alloc op argvs)]
    ['str+ (prim-alloc op (fetch-heads argvs argsptrs))]
    ['str* (prim-alloc op (fetch-heads argvs argsptrs))]
    ['strcmp (prim-alloc op (fetch-heads argvs argsptrs))]
    ['strlen (prim-alloc op (fetch-heads argvs argsptrs))]
    ['strint (prim-alloc op (fetch-heads argvs argsptrs))]
    ['strmin (prim-alloc op (fetch-heads argvs argsptrs))]
    ['strmax (prim-alloc op (fetch-heads argvs argsptrs))]
    ['str-getitem (prim-alloc op (fetch-heads argvs argsptrs))]
    ['strslice (prim-alloc op (fetch-heads argvs argsptrs))]
    ['str-hash (prim-alloc op (fetch-heads argvs argsptrs))]
    ['str= (prim-noalloc op argvs)]
    ['strin (prim-noalloc op argvs)]
    ['strbool (prim-noalloc op (fetch-heads argvs argsptrs))]

    ;list
    ['list+ (prim-alloc op (fetch-heads argvs argsptrs))]
    ['list-extend
      (prim-update op (first argsptrs) (list (first argvs) (second argvs) (third argsptrs)))]
    ['list-len (prim-alloc op (fetch-heads argvs argsptrs))]
    ['list-init (prim-alloc op (fetch-heads argvs argsptrs))]
    ['list-getitem (prim-noalloc op argvs)]
    ['list-remove
      (prim-update op (first argsptrs) (list (first argvs) (second argvs) (third argsptrs)))]
    ['list-setitem
      (prim-update op (first argsptrs) (list (first argvs) (second argvs) (third argsptrs) (fourth argsptrs)))]
    ['list-str (prim-alloc op (fetch-heads argvs argsptrs))]
    ['list-set (prim-alloc op (fetch-heads argvs argsptrs))]
    ['list-tuple (prim-alloc op (fetch-heads argvs argsptrs))]
    ['list-copy (prim-alloc op (fetch-heads argvs argsptrs))]

    ;tuple
    ['tuple+ (prim-alloc op (fetch-heads argvs argsptrs))]
    ['tuple* (prim-alloc op (fetch-heads argvs argsptrs))]
    ['tuple-len (prim-alloc op (fetch-heads argvs argsptrs))]
    ['tuple-getitem (prim-noalloc op argvs)]
    ['tuple-str (prim-alloc op (fetch-heads argvs argsptrs))]
    ['tuple-set (prim-alloc op (fetch-heads argvs argsptrs))]

    ;set
    ['set-len (prim-alloc op (fetch-heads argvs argsptrs))]
    ['set-list (prim-alloc op (fetch-heads argvs argsptrs))]
    ['set-str (prim-alloc op (fetch-heads argvs argsptrs))]

    ;object 
    ['obj-str (prim-alloc op argvs)]

    ;function
    ['is-func? (prim-noalloc op argvs)]

    ;bool
    ['bool-init (prim-alloc op argvs)]

    ; type
    ['type-new (prim-alloc op argvs)]
    ['type-uniqbases (prim-noalloc op argvs)]
    ['type-buildmro (prim-alloc op argvs)]

    ; Returns the class of the given object
    ['$class (prim-noalloc op argvs)]

    ['print (prim-noalloc op argvs)]

    ['Is (prim-alloc op argsptrs)]
    ['IsNot (prim-alloc op argsptrs)]

   [else (prim-noalloc op argsptrs)])))

