#lang plai-typed/untyped

(require "python-core-syntax.rkt"
         "util.rkt"
         "builtins/str.rkt"
         "builtins/list.rkt"
         "builtins/tuple.rkt"
         "builtins/dict.rkt"
         "builtins/set.rkt"
         "builtins/num.rkt"
         "builtins/object.rkt"
         "builtins/bool.rkt"
         "builtins/file.rkt"
         "builtins/type.rkt"
         "builtins/super.rkt"
         "builtins/code.rkt"
         "python-compile.rkt"
         (typed-in racket/string (string-join : ((listof string) string -> string)))
         (typed-in racket/base (format : (string 'a -> string)))
         (typed-in racket/base (number->string : (number -> string)))
         (typed-in racket/base (quotient : (number number -> number)))
         (typed-in racket/base (remainder : (number number -> number)))
         (typed-in racket/base (car : (('a * 'b) -> 'a)))
         (typed-in racket/base (cdr : (('a * 'b) -> 'b)))
         (typed-in racket/list (take : ((listof 'a) number -> (listof 'a))))
         (typed-in racket/list (drop : ((listof 'a) number -> (listof 'a))))
         (typed-in racket/list (last : ((listof 'a) -> 'a)))
         (typed-in racket/base (hash->list : ((hashof 'a 'b) -> (listof ('a * 'b))))))

#|

Since there may end up being a large number of primitives that you
implement for python, here is a suggested factoring into a separate
file.  You can add new primitives here by adding new symbols to the
dispatch.  You might also choose to add more than single-arity
primitives here.

|#

;(require (typed-in racket/base [display : (string -> void)]))

(define (print arg)
  (display (string-append (pretty arg) "\n")))

(define (is-func? argvs env sto)
  (cond
    [(is-fun? (first argvs)) (some true-val)]
    [else (some false-val)]))

(define (num+ args env sto)
  (check-types-pred args env sto MetaNum? MetaNum? 
                        (some (make-builtin-numv (+ (MetaNum-n mval1) 
                                                    (MetaNum-n mval2))))))

(define (num- args env sto)
  (check-types-pred args env sto MetaNum? MetaNum? 
                        (some (make-builtin-numv (- (MetaNum-n mval1) 
                                                   (MetaNum-n mval2))))))
(define (num* args env sto)
        (if (and (some? (VObjectClass-mval (second args)))
                 (MetaStr? (some-v (VObjectClass-mval (second args)))))
            (str* (append (reverse args) (list (fetch-once (some-v (lookup '%str env)) sto)))
                  env sto)
            (check-types-pred args env sto MetaNum? MetaNum? 
                         (some (VObject 'num (some (MetaNum 
                                                    (* (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                       (hash empty))))))
(define (num/ args env sto)
    (check-types-pred args env sto MetaNum? MetaNum? 
                        (some (VObject 'num (some (MetaNum 
                                                    (/ (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (hash empty)))))

(define (num// args env sto)
    (check-types-pred args env sto MetaNum? MetaNum? 
                        (some (VObject 'num (some (MetaNum 
                                                    (quotient (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (hash empty)))))
(define (num% args env sto)
    (check-types-pred args env sto MetaNum? MetaNum? 
                        (some (VObject 'num (some (MetaNum 
                                                    (quotient (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (hash empty)))))

(define (num= args env sto)
    (check-types-pred args env sto MetaNum? MetaNum?
                        (if (= (MetaNum-n mval1) (MetaNum-n mval2))
                          (some true-val)
                          (some false-val))))
(define (num> args env sto)
    (check-types-pred args env sto MetaNum? MetaNum? 
                        (if (> (MetaNum-n mval1) (MetaNum-n mval2))
                          (some true-val)
                          (some false-val))))
(define (num< args env sto)
    (check-types-pred args env sto MetaNum? MetaNum?
                        (if (< (MetaNum-n mval1) (MetaNum-n mval2))
                          (some true-val)
                          (some false-val))))
(define (num>= args env sto)
    (check-types-pred args env sto MetaNum? MetaNum? 
                        (if (>= (MetaNum-n mval1) (MetaNum-n mval2))
                          (some true-val)
                          (some false-val))))
(define (num<= args env sto)
    (check-types-pred args env sto MetaNum? MetaNum? 
                        (if (<= (MetaNum-n mval1) (MetaNum-n mval2))
                         (some true-val)
                          (some false-val))))
(define (numcmp args env sto)
    (check-types-pred args env sto MetaNum? MetaNum?
                        (if (< (MetaNum-n mval1) (MetaNum-n mval2))
                            (some (VObject 'num
                                           (some (MetaNum -1))
                                           (hash empty)))
                            (if (> (MetaNum-n mval1) (MetaNum-n mval2))
                                (some (VObject 'num
                                               (some (MetaNum 1))
                                               (hash empty)))
                                (some (VObject 'num
                                               (some (MetaNum 0))
                                               (hash empty)))))))
(define (num-str args env sto)
    (let ([arg (first args)])
            (some (VObject 'str 
                           (some (MetaStr 
                             (number->string (MetaNum-n (some-v (VObjectClass-mval
                                                                  arg))))))
                           (hash empty)))))

(define (builtin-prim [op : symbol] [argsptrs : (listof CVal)] 
                      [env : Env] [sto : Store] [stk : Stack]) : Result
  (local [
  (define (prim-error msg op args)
    (mk-exception 'TypeError
      (string-append msg (string-append (symbol->string op) (format " ~a" args)))
      env
      sto))
  (define (fetch-heads l1 l2)
    (append (take l1 (- (length l1) 1)) (list (last l2))))
  (define (prim-or-none f args)
    (type-case (optionof CVal) (f args env sto)
      [some (v) (v*s v sto)]
      [none () (alloc-result vnone sto)]))
  (define (prim-or-none-stk f args)
    (type-case (optionof CVal) (f args)
      [some (v) (v*s v sto)]
      [none () (alloc-result vnone sto)]))
  (define (prim-update f to-update args)
    (type-case (optionof CVal) (f args env sto)
      [some (v)
        (type-case CVal to-update
          [VPointer (a) (v*s v (hash-set sto a v))]
          [else
            (prim-error "Non-ptr (update): " op args)])]
      [none ()
        (prim-error "Bad prim (update): " op args)]))
  (define (prim-noalloc f args)
    (type-case (optionof CVal) (f args env sto)
      [some (v) (v*s v sto)]
      [none ()
        (prim-error "Bad prim (noalloc): " op args)]))
  (define (prim-alloc f args)
    (type-case (optionof CVal) (f args env sto)
      [some (v) (alloc-result v sto)]
      [none ()
        (prim-error "Bad prim (alloc): " op args)]))
   ]
  (let ([argvs (map (lambda (a) (fetch-ptr a sto)) argsptrs)])
  (case op
    ['num+ (prim-alloc num+ argvs)]
    ['num- (prim-alloc num- argvs)]
    ['num* (prim-alloc num* argvs)]
    ['num/ (prim-alloc num/ argvs)]
    ['num// (prim-alloc num// argvs)]
    ['num% (prim-alloc num% argvs)]
    ['num= (prim-noalloc num= argvs)]
    ['num> (prim-noalloc num> argvs)]
    ['num< (prim-noalloc num< argvs)]
    ['num>= (prim-noalloc num>= argvs)]
    ['num<= (prim-noalloc num<= argvs)]
    ['numcmp (prim-alloc numcmp argvs)]
    ['num-str (prim-alloc num-str argvs)]

    ;string
    ['str+ (prim-alloc str+ (fetch-heads argvs argsptrs))]
    ['str* (prim-alloc str* (fetch-heads argvs argsptrs))]
    ['strcmp (prim-alloc strcmp (fetch-heads argvs argsptrs))]
    ['strlen (prim-alloc strlen (fetch-heads argvs argsptrs))]
    ['strint (prim-alloc strint (fetch-heads argvs argsptrs))]
    ['strmin (prim-alloc strmin (fetch-heads argvs argsptrs))]
    ['strmax (prim-alloc strmax (fetch-heads argvs argsptrs))]
    ['str-getitem (prim-alloc str-getitem (fetch-heads argvs argsptrs))]
    ['strslice (prim-alloc strslice (fetch-heads argvs argsptrs))]
    ['str-hash (prim-alloc str-hash (fetch-heads argvs argsptrs))]
    ['str= (prim-noalloc streq argvs)]
    ['strin (prim-noalloc strin argvs)]
    ['strbool (prim-noalloc strbool (fetch-heads argvs argsptrs))]

    ;list
    ['list+ (prim-alloc list+ (fetch-heads argvs argsptrs))]
    ['list-extend
      (prim-update list+ (first argsptrs) (list (first argvs) (second argvs) (third argsptrs)))]
    ['list-len (prim-alloc list-len (fetch-heads argvs argsptrs))]
    ['list-init (prim-alloc list-init (fetch-heads argvs argsptrs))]
    ['list-getitem (prim-or-none list-getitem argvs)]
    ['list-remove
      (prim-update list-remove (first argsptrs) (list (first argvs) (second argvs) (third argsptrs)))]
    ['list-setitem
      (prim-update list-setitem (first argsptrs) (list (first argvs) (second argvs) (third argsptrs) (fourth argsptrs)))]
    ['list-str (prim-alloc list-str (fetch-heads argvs argsptrs))]
    ['list-set (prim-alloc list-set (fetch-heads argvs argsptrs))]
    ['list-tuple (prim-alloc list-tuple (fetch-heads argvs argsptrs))]
    ['list-copy (prim-alloc list-copy (fetch-heads argvs argsptrs))]

    ;tuple
    ['tuple+ (prim-alloc tuple+ (fetch-heads argvs argsptrs))]
    ['tuple* (prim-alloc tuple* (fetch-heads argvs argsptrs))]
    ['tuple-len (prim-alloc tuple-len (fetch-heads argvs argsptrs))]
    ['tuple-getitem (prim-or-none tuple-getitem argvs)]
    ['tuple-str (prim-alloc tuple-str (fetch-heads argvs argsptrs))]
    ['tuple-set (prim-alloc tuple-set (fetch-heads argvs argsptrs))]

    ;set
    ['set-len (prim-alloc set-len (fetch-heads argvs argsptrs))]
    ['set-list (prim-alloc set-list (fetch-heads argvs argsptrs))]
    ['set-str (prim-alloc set-str (fetch-heads argvs argsptrs))]

    ;object 
    ['obj-str (prim-alloc obj-str argvs)]

    ;function
    ['is-func? (prim-noalloc is-func? argvs)]

    ;exceptions
    ['exception-str (alloc-result 
                     (let ([arg (first argsptrs)])
                      (VObject 'str
                        (some (MetaStr
                                (pretty-exception arg sto #f)))
                        (hash empty)))
                     sto)]
                            

    ;bool
    ['bool-init (prim-alloc bool-init argvs)]

    ; file
    ['file-open (prim-alloc file-open argvs)]
    ['file-read (prim-alloc file-read argvs)]
    ['file-readall (prim-alloc file-readall argvs)]
    ['file-readline (prim-alloc file-readline argvs)]
    ['file-write (prim-alloc file-write argvs)]
    ['file-close (prim-alloc file-close argvs)]
    ['existing-file? (prim-noalloc existing-file? argvs)]

    ; type
    ['type-new (prim-alloc type-new argvs)]
    ['type-uniqbases (prim-noalloc type-uniqbases argvs)]
    ['type-buildmro (prim-alloc type-buildmro argvs)]

    ; super
    ['super-self (prim-or-none-stk super-self stk)]
    ['super-thisclass (prim-or-none-stk super-thisclass stk)]

    ; Returns the class of the given object
    ['$class (v*s (get-class (first argvs) env sto) sto)]

    ['$locals (begin
               ; (display env) (display "\n\n")
               (if (> (length stk) 0) ;; it must be used inside a function
                   (make-under-dict 
                           (hash
                             (map (lambda (p)
                                    (values (car p) (cdr p)))
                                  (filter (lambda (p)
                                            (not (VUndefined? (fetch-once (cdr p) sto))))
                                          (hash->list (first (Frame-env (first stk)))))))
                           env sto)
                   (error 'locals "Empty stack in locals")))]

    ['code-str (prim-alloc code-str argvs)]
    ['code-globals (prim-alloc code-globals argvs)]

    ['compile (prim-alloc compile (fetch-heads argvs argsptrs))];argvs)]

    ['print (begin (print (first argvs)) (v*s (first argsptrs) sto))]

    ['Is (if (is? (first argsptrs)
                  (second argsptrs) sto)
             (v*s true-val sto)
             (v*s false-val sto))]
    ['IsNot (if (not (is? (first argsptrs)
                  (second argsptrs) sto))
             (v*s true-val sto)
             (v*s false-val sto))]
 
    [else (error 'prim (format "Missed primitive: ~a" op))]))))

