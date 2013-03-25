#lang plai-typed/untyped

(require "python-core-syntax.rkt")

(require [opaque-type-in racket/set [Set set?]])
(require
 (typed-in racket/string (string-join : ((listof string) string -> string)))
 (typed-in racket/base (hash->list : ((hashof 'a 'b)  -> (listof 'c))))
 (typed-in racket/base (number->string : (number -> string)))
 (typed-in racket/base (car : (('a * 'b)  -> 'a)))
 (typed-in racket/base (cdr : (('a * 'b)  -> 'b)))
 (typed-in racket/set (set? : ('a -> boolean)))
 (typed-in racket/set (set->list : (set? -> (listof 'a))))
 (typed-in racket/set (set : ( -> set?)))
 (typed-in racket/set (set-add : (set? 'a -> set?)))
 (typed-in racket/base (exact? : (number -> boolean)))
 
 )
(require [typed-in racket (format : (string 'a -> string))])
(require [typed-in racket (flatten : ((listof (listof 'a) ) -> (listof 'a)))])
(require [typed-in racket (remove-duplicates : ((listof 'a) -> (listof 'a)))])
(require [typed-in racket (memq : ('a (listof 'a) -> (listof 'a)))])
(require [typed-in racket (memf : (('a -> boolean) (listof 'a) -> (listof 'a)))])

(require (typed-in racket/pretty (pretty-print : ('a -> 'b))))

(require (typed-in racket (current-directory : (-> 'b))))
(require (typed-in racket (path->string : ('a -> 'b))))

(print-only-errors #t)

; a file for utility functions that aren't specific to python stuff
(define (pprint exp)
  (pretty-print exp))

; sys.path default value

(define default-builtin-module-paths
  (let ((wd (path->string (current-directory))))
    (list "."
          (string-append
           wd "../tests/modules/")
          (string-append
           wd "../tests/python-reference/modules/"))))

(define (get-module-path)
  default-builtin-module-paths)

(define python-path "/usr/bin/python/")
(define (get-pypath)
  python-path)
(define (set-pypath p)
  (set! python-path p))

(define (list-subtract [big : (listof 'a) ] [small : (listof 'a)] ) : (listof 'a)
  (local
   [(define (list-remove [l : (listof 'a) ] [e : 'a]) : (listof 'a)
      (local [(define (filter-expression [this-elem : 'a] ) : boolean (not (equal? e this-elem)))]
      (filter filter-expression l)))]
   (cond
    [(empty? big) empty]
    [(empty? small) big]
    [else (list-subtract (list-remove big (first small)) (rest small))])))

(define (contains-char? [str : string] [c : char] ) : boolean
  (not (empty? (filter (lambda (x) (char=? x c)) (string->list str)))))

(define (contains-str? [str : string] [s : string] ) : boolean
  (local
   [(define (hlpr [str : (listof char)] [s : (listof char)])
      (cond
       [(empty? s) true]
       [(empty? str) false]
       [(equal? (first s) (first str)) (hlpr (rest str) (rest s))]
       [else (hlpr (rest str) s)]
       ))]
   (hlpr (string->list str) (string->list s))))

(define (chr [str : string] ) : char
  (let ((strlist (string->list str)))
    (if (and (not (empty? strlist)) (empty? (rest strlist)))
        (first strlist)
        (error 'python-desugar:chr (format "cannot convert ~a into a single character" str)))))



(define (list-replace [i : number] [val : 'a] [l : (listof 'a)]) : (listof 'a)
  (cond
    [(empty? l) (error 'util "list-replace out of range")]
    [(= 0 i) (cons val (rest l))]
    [else (cons (first l) (list-replace (- i 1) val (rest l)))]))

(define (seq-ops (ops : (listof CExpr))) : CExpr
  (cond 
    [(= 1 (length ops)) (first ops)]
    [else (CSeq (first ops) 
                (seq-ops (rest ops)))]))

(define (def (class : symbol) (name : symbol) (expr : CExpr)) : CExpr
  (set-field (CId class (GlobalId)) name expr))

(define (VObject [antecedent : symbol] [mval : (optionof MetaVal)]
                 [dict : (hashof 'symbol Address)]) : CVal
  (VObjectClass antecedent mval dict (none)))

(define-syntax (check-types-pred x)
  (syntax-case x ()
    [(_ args env sto tpred1? body)
     (with-syntax ([mval1 (datum->syntax x 'mval1)])
       #'(let ([arg1 (first args)])
           (if (VObjectClass? arg1)
               (let ([mayb-mval1 (VObjectClass-mval arg1)])
                 (if (and (some? mayb-mval1)
                          (tpred1? (some-v (VObjectClass-mval arg1))))
                     (let ([mval1 (some-v mayb-mval1)])
                       body)
                     (none)))
               (none))))]
    [(_ args env sto tpred1? tpred2? body)
     (with-syntax ([mval1 (datum->syntax x 'mval1)]
                   [mval2 (datum->syntax x 'mval2)])
       #'(let ([arg1 (first args)]
               [arg2 (second args)])
           (if (and (VObjectClass? arg1) (VObjectClass? arg2))
               (let ([mayb-mval1 (VObjectClass-mval arg1)]
                     [mayb-mval2 (VObjectClass-mval arg2)])
                 (if (and (some? mayb-mval1) (some? mayb-mval2)
                          (tpred1? (some-v (VObjectClass-mval arg1)))
                          (tpred2? (some-v (VObjectClass-mval arg2))))
                     (let ([mval1 (some-v mayb-mval1)]
                           [mval2 (some-v mayb-mval2)])
                       body)
                     (none)))
               (none))))]
    [(_ args env sto tpred1? tpred2? tpred3? body)
     (with-syntax ([mval1 (datum->syntax x 'mval1)]
                   [mval2 (datum->syntax x 'mval2)]
                   [mval3 (datum->syntax x 'mval3)])
       #'(let ([arg1 (first args)]
               [arg2 (second args)]
               [arg3 (third args)])
           (if (and (VObjectClass? arg1) (VObjectClass? arg2) (VObjectClass? arg3))
               (let ([mayb-mval1 (VObjectClass-mval arg1)]
                     [mayb-mval2 (VObjectClass-mval arg2)]
                     [mayb-mval3 (VObjectClass-mval arg3)])
                 (if (and (some? mayb-mval1) (some? mayb-mval2) (some? mayb-mval3)
                          (tpred1? (some-v (VObjectClass-mval arg1)))
                          (tpred2? (some-v (VObjectClass-mval arg2)))
                          (tpred3? (some-v (VObjectClass-mval arg3))))
                     (let ([mval1 (some-v mayb-mval1)]
                           [mval2 (some-v mayb-mval2)]
                           [mval3 (some-v mayb-mval3)])
                       body)
                     (none)))
               (none))))]))


;; object-is?: returns true if the given o is an object of the given class or somehow a
;; subclass of that one, it uses __mro__ field for multiple inheritance.
;; Reserved for builtin classes, cls should be an id in the builtin scope (%xxx).
(define (object-is? [obj : CVal] [cls : symbol] [env : Env] [sto : Store]) : boolean
  (type-case (optionof Address) (lookup cls env)
    [some (w_cls)
          (let ([obj-cls (get-class obj env sto)]
                [cls (fetch-once w_cls sto)])
            (member cls (get-mro obj-cls sto)))]
    [none ()
          (error 'object-is? (string-append "class not found in env: "
                                            (symbol->string cls)))]))

;; get-mro: fetch-ptr __mro__ field as a list of classes
(define (get-mro [cls : CVal] 
                 [sto : Store]) : (listof CVal)
  (type-case (optionof Address) (hash-ref (VObjectClass-dict (fetch-ptr cls sto)) '__mro__)
    [some (w) (MetaTuple-v (some-v (VObjectClass-mval
                                    (fetch-ptr (fetch-once w sto) sto))))]
    [none () (error 'get-mro (string-append "class without __mro__ field " 
                                            (to-string (fetch-ptr cls sto))))]))

;; option-map: unwrap the option, perform the function (if applicable), re-wrap.
(define (option-map [fn : ('a -> 'b)] [thing : (optionof 'a)]) : (optionof 'b)
    (type-case (optionof 'a) thing
          [some (v) (some (fn v))]
              [none () (none)]))

;; get-class: retrieve the object's class
(define (get-class [obj : CVal] [env : Env] [sto : Store]) : CVal
  (local ([define w_class (if (some? (VObjectClass-class obj))
                              (VObjectClass-class obj)
                              (some (fetch-once (some-v (lookup (VObjectClass-antecedent obj) env)) sto)))])
    (type-case (optionof CVal) w_class
      [some (cv) cv]
      [none () (error 'get-class (string-append "object without class " 
                                                (pretty obj sto)))])))


(define (is? [v1 : CVal]
             [v2 : CVal]
             [s : Store]) : boolean
  (begin
    ;(display v1) (display " ") (display v2) (display " ") (display (eq? v1 v2)) (display "\n")
    (or (eq? v1 v2)
        (and (is-obj-ptr? v1 s) (is-obj-ptr? v2 s)
             (eq? (fetch-ptr v1 s) (fetch-ptr v2 s))))))


(define (pretty arg [sto : Store])
  (type-case CVal arg
    [VObjectClass (a mval d class) (if (some? mval)
                            (pretty-metaval (some-v mval) sto)
                            "Can't print non-builtin object.")]
    [VSym (s) (symbol->string s)]
    [VUndefined () "Undefined"]
    [VPointer (a) (pretty (fetch-ptr arg sto) sto)]))

(define (pretty-metaval (mval : MetaVal) (sto : Store) ) : string
  (let ((pretty (lambda (y) (pretty y sto))))
    (type-case MetaVal mval
      [MetaNum (n) (number->string n)]
      [MetaStr (s) s]
      [MetaClass (c) (string-append "<class "
                                    (string-append (symbol->string c)
                                                   ">"))]
      [MetaList (v) (string-append
                     (string-append "["
                                    (string-join (map pretty v) ", "))
                     "]")]
      [MetaTuple (v) (string-append
                      (string-append "("
                                     (string-join (map pretty v) ", "))
                      ")")]
      [MetaDict (contents)
                (string-append
              (string-append "{"
                             (string-join
                              (map (lambda (pair)
                                      (string-append (pretty (car pair))
                                                     (string-append ": "
                                                                    (pretty (cdr pair)))))
                                   (hash->list contents))
                              ", "))
              "}")]
    [MetaCode (e filename code)
              "<code object>"]
    [MetaNone () "None"]
    [MetaSet (elts)
              (string-append
              (string-append "{"
                             (string-join (map pretty (set->list elts)) ", "))
              "}")]
    [else "builtin-value"]
    )))

(define (pretty-exception [exnptr : CVal] [sto : Store] [print-name : boolean]) : string
  (let ((pretty (lambda (y) (pretty y sto))))
  (local [
          (define exn (fetch-ptr exnptr sto))
          (define name (symbol->string (VObjectClass-antecedent exn)))
          (define args-loc (hash-ref (VObjectClass-dict exn) 'args))
          (define pretty-args (if (some? args-loc)
                                  (string-join 
                                    (map (lambda (v) (pretty (fetch-ptr v sto)))
                                         (MetaTuple-v
                                           (some-v
                                             (VObjectClass-mval
                                               (fetch-ptr
                                                (fetch-once (some-v args-loc) sto) sto)))))
                                    " ")
                                  ""))]
    (if print-name
        (if (not (string=? pretty-args ""))
            (string-append name 
                           (string-append ": "
                                          pretty-args))
            name)
        pretty-args))))

(define (make-exception [name : symbol] [error : string]) : CExpr
  (CLet '$call (LocalId) (py-getfield (CId name (GlobalId)) '__call__)
        (CApp
         (CBuiltinPrim 'obj-getattr (list (CId '$call (LocalId)) (make-builtin-str "__func__")))
         (list (CBuiltinPrim 'obj-getattr (list (CId '$call (LocalId)) (make-builtin-str "__self__")))
               (make-builtin-str error))
         (none))))

(define (default-except-handler [id : symbol] [body : CExpr]) : CExpr
  (CIf (CApp (CId '%isinstance (GlobalId))
             (list (CId id (LocalId))
                   (CId 'BaseException (GlobalId)))
             (none))
       body
       (CId id (LocalId))))

; generates a new unique variable name that isn't allowed by user code 
(define new-id
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (string->symbol (string-append (number->string (unbox n)) "var" ))))))

(define truedummy (VObjectClass 'bool (some (MetaNum 1)) (hash empty) (none)))
(define true-val truedummy)
(define (renew-true env sto)
  (if (or (VObjectClass? true-val)
          (and (is-obj-ptr? true-val sto)
               (VUndefined? (some-v (VObjectClass-class (fetch-ptr true-val sto))))))
      (local [(define with-class
                (VObjectClass 'bool (some (MetaNum 1)) (hash empty)
                              (some (fetch-once (some-v (lookup '%bool env)) sto))))
              (define new-true (alloc-result with-class sto))]
        (begin
          (set! true-val (v*s-v new-true))
          new-true))
      (v*s true-val sto)))

(define falsedummy (VObjectClass 'bool (some (MetaNum 0)) (hash empty) (none)))
(define false-val falsedummy)
(define (renew-false env sto)
  (if (or (VObjectClass? false-val)
          (and (is-obj-ptr? false-val sto)
               (VUndefined? (some-v (VObjectClass-class (fetch-ptr false-val sto))))))
      (local [(define with-class
                (VObjectClass 'bool (some (MetaNum 0)) (hash empty)
                              (some (fetch-once (some-v (lookup '%bool env)) sto))))
              (define new-false (alloc-result with-class sto))]
        (begin
          (set! false-val (v*s-v new-false))
          new-false))
      (v*s false-val sto)))

(define nonedummy (VObjectClass 'NoneType (some (MetaNone)) (hash empty) (none)))
(define vnone nonedummy)
(define (renew-none env sto)
  (if (or (VObjectClass? vnone)
          (and (is-obj-ptr? vnone sto)
               (VUndefined? (some-v (VObjectClass-class (fetch-ptr vnone sto))))))
      (local [(define with-class
                (VObjectClass 'NoneType (some (MetaNone)) (hash empty)
                              (some (fetch-once (some-v (lookup '%NoneType env)) sto))))
              (define new-false (alloc-result with-class sto))]
        (begin
          (set! vnone (v*s-v new-false))
          new-false))
      (v*s vnone sto)))

(define (reset-state)
  (begin
    (set! true-val truedummy)
    (set! false-val falsedummy)
    (set! vnone nonedummy)))

(define (make-set [vals : (listof CVal)]) : Set
  (foldl (lambda (elt st)
                 (set-add st elt))
         (set)
         vals))

;; any: any of a list of boolean (used in the c3 mro algorithm)
(define (any [bs : (listof boolean)]) : boolean
  (foldr (lambda (e1 e2) (or e1 e2)) #f bs))


;; syntactic sugars to avoid writing long expressions in the core language
(define (assign [name : symbol] [expr : CExpr]) : CExpr
  (CAssign (CId name (GlobalId))
           expr))

(define (set-field obj name val)
  (CSetAttr obj (make-pre-str (symbol->string name)) val))

(define (py-num [n : number])
  (CObject (gid '%num) (some (MetaNum n))))

(define (py-len name)
  (py-app (py-getfield (CId name (LocalId)) '__len__)
        (list)
        (none)))

(define (py-getitem name index)
  (py-app (py-getfield (CId name (LocalId)) '__getitem__)
        (list (CObject (gid '%num) (some (MetaNum index))))
        (none)))

(define-syntax pylam
  (syntax-rules ()
    [(_ (arg ...) body)
     (CFunc (list arg ...) (none) (CReturn body) (none))]))

(define-syntax pyapp
  (syntax-rules ()
    [(_ fun arg ...)
     (py-app fun (list arg ...) (none))]))

;; NOTE(dbp): This is for application within CPS'd code, when we _know_ we have CFuncs
(define-syntax pyapp-simple
  (syntax-rules ()
    [(_ fun arg ...)
     (CApp fun (list arg ...) (none))]))


(define (pyget val fld)
  (pyapp (py-getfield val '__getitem__)
               fld))

(define (Id x)
  (CId x (LocalId)))
(define (gid x)
  (CId x (GlobalId)))

(define (Let id val lastbody)
  (CLet id (LocalId) val lastbody))

(define-syntax (Prim stx)
  (syntax-case stx ()
    [(_ op arg ...) #'(CBuiltinPrim op (list arg ...))]))

(define-syntax (Method stx)
  (syntax-case stx ()
    [(_ obj name arg ...) #'(py-app (py-getfield obj name) (list arg ...) (none))]))


 
(define (make-builtin-str [s : string]) : CExpr
  (CObject (gid '%str) (some (MetaStr s))))

(define (make-pre-str [s : string]) : CExpr
  (CObject (CNone) (some (MetaStr s))))

(define (make-builtin-num [n : number]) : CExpr
  (CObject
    (if (exact? n)
        (gid '%int)
        (gid '%float))
    (some (MetaNum n))))

(define-syntax (Construct stx)
  (syntax-case stx ()
    [(_ class arg ...)
     #'(py-app (py-getfield class '__init__) (list arg ...) (none))]))

;; strip the CLet in CModule
(define (get-module-body (es : CExpr)) : CExpr
  (cond [(not (CModule? es))
         (error 'get-module-body "internal error: get-module-body func received a non-module")]
        [else
         (local [(define local-func (CSeq-e1 (CModule-body es))) ; %locals trick
                 (define real-body (CSeq-e2 (CModule-body es)))
                 (define (strip-CLet e)
                   (type-case CExpr e
                              [CLet (x type bind body)
                                    (strip-CLet body)]
                              [else e]))]
            (CSeq local-func
                  (strip-CLet real-body)))]))

;; sinctactic sugar to apply a function or method
(define (py-app [fun : CExpr] [args : (listof CExpr)] [stararg : (optionof CExpr)]) : CExpr
  (CLet '$fun (LocalId) fun
        (CIf (CBuiltinPrim 'is-func? (list (CId '$fun (LocalId))))
             ;; function call
             (CApp (CId '$fun (LocalId)) args stararg)
             ;; try to get __call__ attribute
             (CTryExceptElse
              (py-getfield (CId '$fun (LocalId)) '__call__)
              ;; exception raised, not callable
              '_ (CRaise (some (make-exception 'TypeError "object is not callable")))
              ;; no exception, __call__ is available
              (CLet '$call (LocalId) (py-getfield (CId '$fun (LocalId)) '__call__)
                    (CIf (CBuiltinPrim 'is-func? (list (CId '$call (LocalId))))
                         ;; __call__ is a function
                         (CApp (CId '$call (LocalId)) args stararg)
                         ;; else it should be a method, call __func__ and pass __self__ as first arg.
                         (CApp (CBuiltinPrim 'obj-getattr (list (CId '$call (LocalId))
                                                                (make-builtin-str "__func__")))
                               (cons (CBuiltinPrim 'obj-getattr (list (CId '$call (LocalId))
                                                                      (make-builtin-str "__self__")))
                                     args)
                               stararg)))))))


(define (py-setfield obj attr val)
  (CLet '$obj (LocalId) obj
    (CApp (CGetAttr
            (CBuiltinPrim '$class (list (CId '$obj (LocalId))))
            (make-pre-str "__setattr__"))
          (list
           (CId '$obj (LocalId))
           (make-pre-str (symbol->string attr))
           val)
          (none))))

;; sintactic sugar to get a field from an object
(define (py-getfield [obj-exp : CExpr] [attr : symbol]) : CExpr
  (local [(define (is-special-method? [n : symbol])
            (member n (list '__in__ '__call__ '__eq__ '__cmp__ '__str__ '__getitem__ '__gt__ '__lt__ '__lte__ '__gte__)))]
    (cond
      [(eq? attr '__class__)
       ;; special attribute __class__, cannot be overriden.
       (CBuiltinPrim '$class (list obj-exp))]
      [(eq? attr '__dict__)
       ;; special attribute __dict__, cannot be overriden.
       (CApp (CId '%obj_dict (GlobalId)) (list obj-exp) (none))]
      [(eq? attr '__mro__)
       ;; special attribute __mro__, cannot be overriden.
       (CBuiltinPrim 'obj-getattr (list obj-exp (make-builtin-str "__mro__")))]
      [(is-special-method? attr)
       ;; special methods cannot be overriden by __getattribute__, call %special_getattr(obj, attr)
       (CApp (CId '%special_getattr (GlobalId)) (list obj-exp (make-builtin-str (symbol->string attr))) (none))]
      [else
       ;; normal case, call type(obj).__getattribute__(obj, attr), this can be overriden
       (CLet '$obj (LocalId) obj-exp
             (CLet '$cls (LocalId) (CBuiltinPrim '$class (list (CId '$obj (LocalId))))
                   (CApp (CGetAttr (CId '$cls (LocalId)) (make-builtin-str "__getattribute__"))
                         (list (CId '$obj (LocalId)) (make-builtin-str (symbol->string attr))) (none))))])))
