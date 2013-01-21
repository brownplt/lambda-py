#lang plai-typed 

(require "python-core-syntax.rkt")

(require [opaque-type-in racket/set [Set set?]])
(require
 (typed-in racket/string (string-join : ((listof string) string -> string)))
 (typed-in racket/base (hash-for-each : ((hashof 'a 'b) ('c 'd -> 'e) -> void)))
 (typed-in racket/base (hash->list : ((hashof 'a 'b)  -> (listof 'c))))
 (typed-in racket/base (number->string : (number -> string)))
 (typed-in racket/base (car : (('a * 'b)  -> 'a)))
 (typed-in racket/base (cdr : (('a * 'b)  -> 'b)))
 (typed-in racket/set (set? : ('a -> boolean)))
 (typed-in racket/set (set->list : (set? -> (listof 'a))))
 (typed-in racket/set (set : ( -> set?)))
 (typed-in racket/set (set-add : (set? 'a -> set?)))
 
 )
(require [typed-in racket (format : (string 'a -> string))])
(require [typed-in racket (flatten : ((listof (listof 'a) ) -> (listof 'a)))])
(require [typed-in racket (remove-duplicates : ((listof 'a) -> (listof 'a)))])


(print-only-errors #t)

; a file for utility functions that aren't specific to python stuff

(define python-path "/usr/local/bin/python3.2")
(define (get-pypath)
  python-path)
(define (set-pypath p)
  (set! python-path p))

; lists->hash - given two parallel list produce a mutable hash mapping 
; values from one to values in the other
(define (lists->hash [l1 : (listof 'a)] [l2 : (listof 'b)]) : (hashof 'a 'b)
  (local [(define h (make-hash empty))]
    (begin
      (map2 (lambda (k v) (hash-set! h k v))
            l1 l2)
      h)))


(test (lists->hash (list "a" "b" "c") (list 1 2 3))
      (let ([h (make-hash empty)])
        (begin
          (hash-set! h "a" 1)
          (hash-set! h "b" 2)
          (hash-set! h "c" 3) 
          h)))

(define (make-exception-class [name : symbol]) : CExpr
  (CClass
    name
    (list 'Exception)
    (CNone)))

(define (assign [name : symbol] [expr : CExpr]) : CExpr
  (CAssign (CId name (GlobalId))
           expr))

(define (list-subtract [big : (listof 'a) ] [small : (listof 'a)] ) : (listof 'a)
  (local
   [(define (list-remove [l : (listof 'a) ] [e : 'a]) : (listof 'a)
      (local [(define (filter-expression [this-elem : 'a] ) : boolean (not (equal? e this-elem)))]
      (filter filter-expression l)))]
   (cond
    [(empty? big) empty]
    [(empty? small) big]
    [else (list-subtract (list-remove big (first small)) (rest small))])))

(test (list-subtract (list 'a 'b 'c 'd 'e) (list 'c 'e)) (list 'a 'b 'd))

(define (contains-char? [str : string] [c : char] ) : boolean
  (not (empty? (filter (lambda (x) (char=? x c)) (string->list str)))))

(define (chr [str : string] ) : char
  (let ((strlist (string->list str)))
    (if (and (not (empty? strlist)) (empty? (rest strlist)))
        (first strlist)
        (error 'python-desugar:chr (format "cannot convert ~a into a single character" str)))))

(test/exn (chr "hi") "cannot convert hi into a single character")
(test/exn (chr "") "cannot convert  into a single character")

(test (contains-char? "hello" (chr "l") ) true)
(test (contains-char? "jasfdjoiewfa" (chr "-") ) false)
(test (contains-char? "custom-identifier" (chr "-")) true)
(test (contains-char? "customidentifier" (chr "-")) false)



(define (list-replace [i : number] [val : 'a] [l : (listof 'a)]) : (listof 'a)
  (cond
    [(empty? l) (error 'util "list-replace out of range")]
    [(= 0 i) (cons val (rest l))]
    [else (cons (first l) (list-replace (- i 1) val (rest l)))]))
(test (list-replace 2 63 (list 1 2 3 4)) (list 1 2 63 4))

(define (immutable-hash-copy h)
  (let ([r (hash empty)])
    (begin
      (hash-for-each h (lambda (k v) (set! r (hash-set r k v))))
      r)))

(define (seq-ops (ops : (listof CExpr))) : CExpr
  (cond 
    [(= 1 (length ops)) (first ops)]
    [else (CSeq (first ops) 
                (seq-ops (rest ops)))]))

(define (def (class : symbol) (name : symbol) (expr : CExpr)) : CExpr
  (CAssign (CGetField (CId class (GlobalId)) name) expr))


; Macro to match varargs
; Example:
;
; (match-varargs 'args
;   [() (CObject 'num (some (MetaNum 0)))]
;   [('a) (CId 'a (LocalId))]
;   [('a 'b) (CBuiltinPrim 'num+ (CId 'a (LocalId)) (CId 'b (LocalId)))])
;
(define-syntax (match-varargs x)
  (syntax-case x ()
    [(match-varargs 'args [vars body])
     #'(if-varargs 'args vars body)]
    [(match-varargs 'args [vars1 body1] [vars2 body2])
     #'(if-varargs 'args vars1 body1
                 (if-varargs 'args vars2 body2))]
    [(match-varargs 'args [vars1 body1] [vars2 body2] [vars3 body3])
     #'(if-varargs 'args vars1 body1
                 (if-varargs 'args vars2 body2
                             (if-varargs 'args vars3 body3)))]))

; Helper macro used in implementing match-varargs
(define-syntax (if-varargs x)
  (syntax-case x ()
    [(if-varargs 'args vars body)
     #'(if-varargs 'args vars body (CError (CStr "argument mismatch")))]
    [(if-varargs 'args () body else-part)
     #'(CIf ; Did we get 0 args?
        (CBuiltinPrim 'num= (list (py-len 'args) (py-num 0)))
        body
        else-part)]
    [(if-varargs 'args (a) body else-part)
     #'(CIf ; Did we get 1 args?
        (CBuiltinPrim 'num= (list (py-len 'args) (py-num 1)))
        (CLet a (LocalId) (py-getitem 'args 0)
              body)
        else-part)]
    [(if-varargs 'args (a b) body else-part)
     #'(CIf ; Did we get 2 args?
        (CBuiltinPrim 'num= (list (py-len 'args) (py-num 2)))
        (CLet a (LocalId) (py-getitem 'args 0)
              (CLet b (LocalId) (py-getitem 'args 1)
                    body))
        else-part)]))

;; syntactic sugars to avoid writing long expressions in the core language
(define (py-num [n : number])
  (CObject 'num (some (MetaNum n))))

(define (py-len name)
  (CApp (CGetField (CId name (LocalId)) '__len__)
        (list (CId name (LocalId)))
        (none)))

(define (py-getitem name index)
  (CApp (CGetField (CId name (LocalId)) '__getitem__)
        (list (CId name (LocalId))
              (CObject 'num (some (MetaNum index))))
        (none)))

;; the copypasta here is bad but we aren't clever enough with macros
(define-syntax (check-types x)
  (syntax-case x ()
    [(check-types args env sto t1 body)
     (with-syntax ([mval1 (datum->syntax x 'mval1)])
       #'(let ([arg1 (first args)])
           (if (and (VObject? arg1) (object-is? arg1 t1 env sto))
               (let ([mayb-mval1 (VObject-mval arg1)])
                 (if (some? mayb-mval1)
                     (let ([mval1 (some-v mayb-mval1)])
                       body)
                     (none)))
               (none))))]
    [(check-types args env sto t1 t2 body)
     (with-syntax ([mval1 (datum->syntax x 'mval1)]
                   [mval2 (datum->syntax x 'mval2)])
       #'(let ([arg1 (first args)]
               [arg2 (second args)])
           (if (and (VObject? arg1) (VObject? arg2)
                    (object-is? arg1 t1 env sto)
                    (object-is? arg2 t2 env sto))
               (let ([mayb-mval1 (VObject-mval arg1)]
                     [mayb-mval2 (VObject-mval arg2)])
                 (if (and (some? mayb-mval1) (some? mayb-mval2))
                     (let ([mval1 (some-v mayb-mval1)]
                           [mval2 (some-v mayb-mval2)])
                       body)
                     (none)))
               (none))))]
    [(check-types args env sto t1 t2 t3 body)
     (with-syntax ([mval1 (datum->syntax x 'mval1)]
                   [mval2 (datum->syntax x 'mval2)]
                   [mval3 (datum->syntax x 'mval3)])
       #'(let ([arg1 (first args)]
               [arg2 (second args)]
               [arg3 (third args)])
           (if (and (VObject? arg1) (VObject? arg2)
                    (VObject? arg3)
                    (object-is? arg1 t1 env sto)
                    (object-is? arg2 t2 env sto)
                    (object-is? arg3 t3 env sto))
               (let ([mayb-mval1 (VObject-mval arg1)]
                     [mayb-mval2 (VObject-mval arg2)]
                     [mayb-mval3 (VObject-mval arg3)])
                 (if (and (some? mayb-mval1) 
                          (some? mayb-mval2)
                          (some? mayb-mval3))
                     (let ([mval1 (some-v mayb-mval1)]
                           [mval2 (some-v mayb-mval2)]
                           [mval3 (some-v mayb-mval3)])
                       body)
                     (none)))
               (none))))]))

;; returns true if the given o is an object of the given class or somehow a
;; subclass of that one. Modified to look at __mro__ for multiple inheritance. 
(define (object-is? [o : CVal] [c : symbol] [env : Env] [s : Store]) : boolean
  (let ([obj-cls (fetch (some-v (lookup (VObject-antecedent o) env)) s)]
        [cls (fetch (some-v (lookup c env)) s)])
    (member cls (get-mro obj-cls s))))


;; get-mro: fetch __mro__ field as a list of classes
;; termporarily prepended with cls to avoid self reference in __mro__
(define (get-mro [cls : CVal] [sto : Store]) : (listof CVal)
  (type-case (optionof Address) (hash-ref (VObject-dict cls) '__mro__)
    [some (w) (cons cls (MetaTuple-v (some-v (VObject-mval (fetch w sto)))))]
    [none () (error 'get-mro (string-append "class without __mro__ field " 
                                            (pretty cls)))]))
(define (is? [v1 : CVal]
             [v2 : CVal]) : boolean
  (begin
    ;(display v1) (display " ") (display v2) (display " ") (display (eq? v1 v2)) (display "\n")
    (eq? v1 v2)))

(define (pretty arg)
  (type-case CVal arg
    [VObject (a mval d) (if (some? mval)
                            (pretty-metaval (some-v mval))
                            "Can't print non-builtin object.")]
    [VClosure (env args sarg body) "<function>"]
    [VUndefined () "Undefined"]
    [VPointer (a) (string-append "Pointer to address " (number->string a))]))

(define (pretty-metaval (mval : MetaVal)) : string
  (type-case MetaVal mval
    [MetaNum (n) (number->string n)]
    [MetaStr (s) s]
    [MetaClass (c) (symbol->string c)]
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
    [MetaNone () "None"]
    [MetaSet (elts)
              (string-append
              (string-append "{"
                             (string-join (map pretty (set->list elts)) ", "))
              "}")]
    [else "builtin-value"]
    ))

(define (pretty-exception [exn : CVal] [sto : Store]) : string
  (local [(define name (symbol->string (VObject-antecedent exn)))
          (define args
            (begin ;(display (VObject-dict exn)) (display "\n")
                   ;(display (VObject-mval (fetch (some-v (hash-ref (VObject-dict exn) 'args)) sto)))
                   ;(display "\n")
            (string-join 
              (map pretty
                   (MetaTuple-v
                     (some-v (VObject-mval
                               (fetch (some-v (hash-ref (VObject-dict exn) 'args)) sto)))))
              " ")))]
    (if (not (string=? args ""))
        (string-append name 
                       (string-append ": "
                                      args))
        name)))

(define (make-exception [name : symbol] [error : string]) : CExpr
  (CApp
    (CId name (GlobalId))
    (list (CStr error))
    (none)))

; generates a new unique variable name that isn't allowed by user code 
(define new-id
  (let ([n (box 0)])
    (lambda ()
      (begin
        (set-box! n (add1 (unbox n)))
        (string->symbol (string-append (number->string (unbox n)) "var" ))))))

(define true-val
  (VObject
    'bool
    (some (MetaNum 1))
    (hash empty)))

(define false-val
  (VObject
    'bool
    (some (MetaNum 0))
    (hash empty)))

(define (get-optionof-field [n : symbol] [c : CVal] [e : Env] [s : Store]) : (optionof CVal)
  (begin ;(display n) (display " -- ") (display c) (display "\n") (display e) (display "\n\n")
  (type-case CVal c
    [VObject (antecedent mval d) 
                    (let ([w (hash-ref (VObject-dict c) n)])
              (type-case (optionof Address) w
                [some (w) (some (fetch w s))]
                [none () (let ([mayb-base (lookup antecedent e)])
                           (if (some? mayb-base)
                             (let ([base (fetch (some-v mayb-base) s)])
                                 (get-optionof-field n base e s))
                                           (none)))]))]
    [else (error 'interp "Not an object with functions.")])))

(define (make-set [vals : (listof CVal)]) : Set
  (foldl (lambda (elt st)
                 (set-add st elt))
         (set)
         vals))

;; any: any of a list of boolean (used in the c3 mro algorithm)
(define (any [bs : (listof boolean)]) : boolean
  (foldr (lambda (e1 e2) (or e1 e2)) #f bs))
