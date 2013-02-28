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
         (typed-in racket/string (string-join : ((listof string) string -> string)))
         (typed-in racket/base (format : (string 'a -> string)))
         (typed-in racket/base (number->string : (number -> string)))
         (typed-in racket/base (quotient : (number number -> number)))
         (typed-in racket/base (remainder : (number number -> number)))
         (typed-in racket/base (car : (('a * 'b) -> 'a)))
         (typed-in racket/base (cdr : (('a * 'b) -> 'b)))
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

(define (builtin-prim [op : symbol] [args : (listof CVal)] 
                      [env : Env] [sto : Store] [stk : Stack]) : (optionof CVal)
  (case op
    ['num+ (check-types args env sto 'num 'num 
                        (some (make-builtin-numv (+ (MetaNum-n mval1) 
                                                   (MetaNum-n mval2)))))]
    ['num- (check-types args env sto 'num 'num 
                        (some (make-builtin-numv (- (MetaNum-n mval1) 
                                                   (MetaNum-n mval2)))))]
    ['num*
        (if (and (some? (VObjectClass-mval (second args)))
                 (MetaStr? (some-v (VObjectClass-mval (second args)))))
            (builtin-prim 'str* (reverse args) env sto stk)
            (check-types args env sto 'num 'num 
                         (some (VObject 'num (some (MetaNum 
                                                    (* (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                       (hash empty)))))]
    ['num/ (check-types args env sto 'num 'num 
                        (some (VObject 'num (some (MetaNum 
                                                    (/ (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (hash empty))))]
    ['num// (check-types args env sto 'num 'num 
                        (some (VObject 'num (some (MetaNum 
                                                    (quotient (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (hash empty))))]
    ['num% (check-types args env sto 'num 'num 
                        (some (VObject 'num (some (MetaNum 
                                                    (quotient (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (hash empty))))]
    ['num= (check-types args env sto 'num 'num 
                        (if (= (MetaNum-n mval1) (MetaNum-n mval2))
                          (some true-val)
                          (some false-val)))]
    ['num> (check-types args env sto 'num 'num 
                        (if (> (MetaNum-n mval1) (MetaNum-n mval2))
                          (some true-val)
                          (some false-val)))]

    ['num< (check-types args env sto 'num 'num 
                        (if (< (MetaNum-n mval1) (MetaNum-n mval2))
                          (some true-val)
                          (some false-val)))]

    ['num>= (check-types args env sto 'num 'num 
                        (if (>= (MetaNum-n mval1) (MetaNum-n mval2))
                          (some true-val)
                          (some false-val)))]

    ['num<= (check-types args env sto 'num 'num 
                        (if (<= (MetaNum-n mval1) (MetaNum-n mval2))
                         (some true-val)
                          (some false-val)))]
    
    ['numcmp (check-types args env sto 'num 'num
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
                                               (hash empty))))))]
    
    ['num-str (let ([arg (first args)])
            (some (VObject 'str 
                           (some (MetaStr 
                             (number->string (MetaNum-n (some-v (VObjectClass-mval
                                                                  arg))))))
                           (hash empty))))]
    ;string
    ['str+ (str+ args env sto)]
    ['str= (streq args env sto)]
    ['str* (str* args env sto)]
    ['strcmp (strcmp args env sto)]
    ['strlen (strlen args env sto)]
    ['strbool (strbool args env sto)]
    ['strint (strint args env sto)]
    ['strmin (strmin args env sto)]
    ['strmax (strmax args env sto)]
    ['strin (strin args env sto)]
    ['str-getitem (str-getitem args env sto)]
    ['strlist (strlist args env sto)]
    ['str-tuple (str-tuple args env sto)]
    ['strslice (strslice args env sto)]

    ;list
    ['list+ (list+ args env sto)]
    ['list-len (list-len args env sto)]
    ['list-in (list-in args env sto)]
    ['list-getitem (list-getitem args env sto)]
    ['list-setitem (list-setitem args env sto)]
    ['list-str (list-str args env sto)]
    ['list-set (list-set args env sto)]
    ['list-tuple (list-tuple args env sto)]
    ['list-copy (list-copy args env sto)]

    ;tuple
    ['tuple+ (tuple+ args env sto)]
    ['tuple* (tuple* args env sto)]
    ['tuple-len (tuple-len args env sto)]
    ['tuple-getitem (tuple-getitem args env sto)]
    ['tuple-str (tuple-str args env sto)]

    ;dict
    ['dict-len (dict-len args env sto)]
    ['dict-str (dict-str args env sto)]
    ['dict-clear (dict-clear args env sto)]
    ['dict-in (dict-in args env sto)]
    ['dict-update (dict-update args env sto)]
    ['dict-get (dict-get args env sto)]
    ['dict-keys (dict-keys args env sto)]
    ['dict-values (dict-values args env sto)]
    ['dict-items (dict-items args env sto)]
    ['dict-getitem (dict-getitem args env sto)]
    ['dict-setitem (dict-setitem args env sto)]
    ['dict-delitem (dict-delitem args env sto)]
    ['dict->list (dict->list args env sto)]
    ['dict-init (dict-init args env sto)]

    ;set
    ['set-len (set-len args env sto)]
    ['set-eq (set-eq args env sto)]
    ['set-in (set-in args env sto)]
    ['set-sub (set-sub args env sto)]
    ['set-and (set-and args env sto)]
    ['set-or (set-or args env sto)]
    ['set-xor (set-xor args env sto)]
    ['set-list (set-list args env sto)]

    ;object 
    ['obj-str (obj-str args)]

    ;exceptions
    ['exception-str (let ([arg (first args)])
                      (some (VObject 'str
                        (some (MetaStr
                                (pretty-exception arg sto #f)))
                        (hash empty))))]
                            

    ;bool
    ['bool-init (bool-init args env sto)]

    ; file
    ['file-open (file-open args env sto)]
    ['file-read (file-read args env sto)]
    ['file-readall (file-readall args env sto)]
    ['file-readline (file-readline args env sto)]
    ['file-write (file-write args env sto)]
    ['file-close (file-close args env sto)]

    ;isinstance
    ['isinstance 
     (if (or (none? (VObjectClass-mval (second args)))
             (not (MetaClass? (some-v (VObjectClass-mval (second args))))))
         (none)
         (if (object-is-cls? (first args) (second args) env sto)
             (some true-val)
             (some false-val)))]

    ; Returns the class of the given object
    ['$class
     (some (get-class (first args) env sto))]

    ['$locals (begin
               ; (display env) (display "\n\n")
               (if (> (length stk) 0) ;; it must be used inside a function
                   (some (make-under-dict 
                           (hash
                             (map (lambda (p)
                                    (values (car p) (cdr p)))
                                  (filter (lambda (p)
                                            (not (VUndefined? (fetch (cdr p) sto))))
                                          (hash->list (first (Frame-env (first stk)))))))
                           sto))
                   (none)))]

    ['$self ;; returns the active self, if any, from the stack
     (local [(define (fetch-self [st : Stack]) : (optionof CVal)
               (cond
                 [(empty? st) (none)]
                 [(some? (Frame-self (first st))) (Frame-self (first st))]
                 [else (fetch-self (rest st))]))]
       (fetch-self stk))]

    ['$thisclass ;; returns the embodying class, if any, from the stack
     (local [(define (fetch-class [st : Stack]) : (optionof CVal)
               (cond
                 [(empty? st) (none)]
                 [(some? (Frame-class (first st))) (Frame-class (first st))]
                 [else (fetch-class (rest st))]))]
       (fetch-class stk))]
    [else (error 'prim (format "Missed primitive: ~a" op))]
))
