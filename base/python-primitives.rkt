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

(define (callable [arg : CVal]) : CVal 
  (type-case CVal arg
    [VClosure (e a v b o) true-val]             
    [VObjectClass (a m d c)
                  (if (some? m)
                      (if (MetaClass? (some-v m))
                          true-val
                          false-val)
                      false-val)]
    [else false-val]))


(define (python-prim1 op arg)
  (case op
    [(print) (begin (print arg) arg)]
    [(callable) (callable arg)]))

(define (num+ args env sto)
  (check-types args env sto 'num 'num 
                        (some (make-builtin-numv (+ (MetaNum-n mval1) 
                                                    (MetaNum-n mval2))))))

(define (num- args env sto)
  (check-types args env sto 'num 'num 
                        (some (make-builtin-numv (- (MetaNum-n mval1) 
                                                   (MetaNum-n mval2))))))
(define (num* args env sto)
        (if (and (some? (VObjectClass-mval (second args)))
                 (MetaStr? (some-v (VObjectClass-mval (second args)))))
            (some-v (str* (reverse args) env sto))
            (check-types args env sto 'num 'num 
                         (some (VObject 'num (some (MetaNum 
                                                    (* (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                       (hash empty))))))
(define (num/ args env sto)
    (check-types args env sto 'num 'num 
                        (some (VObject 'num (some (MetaNum 
                                                    (/ (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (hash empty)))))

(define (num// args env sto)
    (check-types args env sto 'num 'num 
                        (some (VObject 'num (some (MetaNum 
                                                    (quotient (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (hash empty)))))
(define (num% args env sto)
    (check-types args env sto 'num 'num 
                        (some (VObject 'num (some (MetaNum 
                                                    (quotient (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (hash empty)))))

(define (num= args env sto)
    (check-types args env sto 'num 'num 
                        (if (= (MetaNum-n mval1) (MetaNum-n mval2))
                          (some true-val)
                          (some false-val))))
(define (num> args env sto)
    (check-types args env sto 'num 'num 
                        (if (> (MetaNum-n mval1) (MetaNum-n mval2))
                          (some true-val)
                          (some false-val))))
(define (num< args env sto)
    (check-types args env sto 'num 'num 
                        (if (< (MetaNum-n mval1) (MetaNum-n mval2))
                          (some true-val)
                          (some false-val))))
(define (num>= args env sto)
    (check-types args env sto 'num 'num 
                        (if (>= (MetaNum-n mval1) (MetaNum-n mval2))
                          (some true-val)
                          (some false-val))))
(define (num<= args env sto)
    (check-types args env sto 'num 'num 
                        (if (<= (MetaNum-n mval1) (MetaNum-n mval2))
                         (some true-val)
                          (some false-val))))
(define (numcmp args env sto)
    (check-types args env sto 'num 'num
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
  (define (fetch-heads l1 l2)
    (append (take l1 (- (length l1) 1)) (list (last l2))))
  (define (prim-noalloc f args)
    (type-case (optionof CVal) (f args env sto)
      [some (v) (v*s v sto)]
      [none () (Exception (make-builtin-str "Bad prim (noalloc)") sto)]))
  (define (prim-alloc f args)
    (type-case (optionof CVal) (f args env sto)
      [some (v) (alloc-result v sto)]
      [none () (Exception (make-builtin-str "Bad prim (alloc)") sto)]))
   ]
  (let ([argvs (map (lambda (a) (fetch-ptr a sto)) argsptrs)])
  (case op
    ['num+ (prim-alloc num+ argvs)]
    ['num- (prim-alloc num- argvs)]
    ['num* (prim-alloc num* argvs)]
    ['num/ (prim-alloc num/ argvs)]
    ['num// (prim-alloc num// argvs)]
    ['num% (prim-alloc num% argvs)]
    ['num= (prim-alloc num= argvs)]
    ['num> (prim-alloc num> argvs)]
    ['num< (prim-alloc num< argvs)]
    ['num>= (prim-alloc num>= argvs)]
    ['num<= (prim-alloc num<= argvs)]
    ['numcmp (prim-alloc numcmp argvs)]
    ['num-str (prim-alloc num-str argvs)]

    ;string
    ['str+ (prim-alloc str+ argvs)]
    ['str= (prim-alloc streq argvs)]
    ['str* (prim-alloc str* argvs)]
    ['strcmp (prim-alloc strcmp argvs)]
    ['strlen (prim-alloc strlen argvs)]
    ['strbool (prim-alloc strbool argvs)]
    ['strint (prim-alloc strint argvs)]
    ['strmin (prim-alloc strmin argvs)]
    ['strmax (prim-alloc strmax argvs)]
    ['strin (prim-alloc strin argvs)]
    ['str-getitem (prim-alloc str-getitem argvs)]
    ['strlist (prim-alloc strlist argvs)]
    ['str-tuple (prim-alloc str-tuple argvs)]
    ['strslice (prim-alloc strslice argvs)]

    ;list
    ['list+ (prim-alloc list+ (fetch-heads argvs argsptrs))]
    ['list-len (prim-alloc list-len (fetch-heads argvs argsptrs))]
    ['list-in (prim-noalloc list-in argvs)]
    ['list-init (prim-alloc list-in (fetch-heads argvs argsptrs))]
    ['list-getitem (prim-noalloc list-getitem argvs)]
    ['list-setitem (prim-noalloc list-setitem (fetch-heads argvs argsptrs))]
    ['list-str (prim-alloc list-str (fetch-heads argvs argsptrs))]
    ['list-set (prim-alloc list-set (fetch-heads argvs argsptrs))]
    ['list-tuple (prim-alloc list-tuple (fetch-heads argvs argsptrs))]
    ['list-copy (prim-alloc list-copy (fetch-heads argvs argsptrs))]

    ;tuple
    ['tuple+ (prim-alloc tuple+ (fetch-heads argvs argsptrs))]
    ['tuple* (prim-alloc tuple* (fetch-heads argvs argsptrs))]
    ['tuple-len (prim-alloc tuple-len (fetch-heads argvs argsptrs))]
    ['tuple-getitem (let [(res (prim-noalloc tuple-getitem argvs))]
                      (begin
                        (display "Getitem args: ") (display argvs) (display "\n\n")
                        (display "Getitem res: ") (display (v*s-v res)) (display "\n\n")
                        res))]
    ['tuple-str (prim-alloc tuple-str (fetch-heads argvs argsptrs))]

    ;dict
    ['dict-len (prim-alloc dict-len (fetch-heads argvs argsptrs))]
    ['dict-str (prim-alloc dict-str (fetch-heads argvs argsptrs))]
    ['dict-keys (prim-alloc dict-keys (fetch-heads argvs argsptrs))]
    ['dict-values (prim-alloc dict-values (fetch-heads argvs argsptrs))]
    ['dict-items (prim-alloc dict-items (fetch-heads argvs argsptrs))]
    ['dict->list (prim-alloc dict->list (fetch-heads argvs argsptrs))]
    ['dict-init (prim-alloc dict-init (fetch-heads argvs argsptrs))]
    ['dict-getitem (prim-noalloc dict-getitem argvs)]
    ['dict-setitem (prim-noalloc dict-setitem (fetch-heads argvs argsptrs))]
    ['dict-delitem (prim-noalloc dict-delitem argvs)]
    ['dict-clear (prim-noalloc dict-clear argvs)]
    ['dict-in (prim-noalloc dict-in argvs)]
    ['dict-update (prim-noalloc dict-update argvs)]
    ['dict-get (prim-noalloc dict-get argvs)]

    ;set
    ['set-len (prim-alloc set-len (fetch-heads argvs argsptrs))]
    ['set-eq (prim-alloc set-eq (fetch-heads argvs argsptrs))]
    ['set-sub (prim-alloc set-sub (fetch-heads argvs argsptrs))]
    ['set-and (prim-alloc set-and (fetch-heads argvs argsptrs))]
    ['set-or (prim-alloc set-or (fetch-heads argvs argsptrs))]
    ['set-xor (prim-alloc set-xor (fetch-heads argvs argsptrs))]
    ['set-list (prim-alloc set-list (fetch-heads argvs argsptrs))]
    ['set-in (prim-noalloc set-in argvs)]

    ;object 
    ['obj-str (prim-alloc obj-str argvs)]

    ;exceptions
    ['exception-str (alloc-result 
                     (let ([arg (first argvs)])
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

    ; super
    ['super-self (alloc-result (some-v (super-self stk)) sto)]
    ['super-thisclass (alloc-result (some-v (super-thisclass stk)) sto)]

    ; Returns the class of the given object
    ['$class (alloc-result
              (get-class (first argvs) env sto)
              sto)]

    ['$locals (alloc-result 
                (begin
               ; (display env) (display "\n\n")
               (if (> (length stk) 0) ;; it must be used inside a function
                   (make-under-dict 
                           (hash
                             (map (lambda (p)
                                    (values (car p) (cdr p)))
                                  (filter (lambda (p)
                                            (not (VUndefined? (fetch (cdr p) sto))))
                                          (hash->list (first (Frame-env (first stk)))))))
                           sto)
                   (error 'locals "Empty stack in locals")))
                   sto)]

    ['code-str (prim-alloc code-str argvs)]
    ['code-globals (prim-alloc code-globals argvs)]

    ['compile (prim-alloc compile argvs)]
    
    [else (error 'prim (format "Missed primitive: ~a" op))]
))))
