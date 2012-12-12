#lang plai-typed

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
         (typed-in racket/string (string-join : ((listof string) string -> string)))
         (typed-in racket/base (number->string : (number -> string)))
         (typed-in racket/base (quotient : (number number -> number)))
         (typed-in racket/base (remainder : (number number -> number))))

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
    [VClosure (e a v b) true-val]
    [VObject (a m d) (if (some? m)
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
                      [env : Env] [sto : Store]) : (optionof CVal)
  (case op
    ['num+ (check-types args env sto 'num 'num 
                        (some (make-builtin-numv (+ (MetaNum-n mval1) 
                                                   (MetaNum-n mval2)))))]
    ['num- (check-types args env sto 'num 'num 
                        (some (make-builtin-numv (- (MetaNum-n mval1) 
                                                   (MetaNum-n mval2)))))]
    ['num*
        (if (and (some? (VObject-mval (second args)))
              (MetaStr? (some-v (VObject-mval (second args)))))
          (builtin-prim 'str* (reverse args) env sto)
          (check-types args env sto 'num 'num 
                        (some (VObject 'num (some (MetaNum 
                                                    (* (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                       (make-hash empty)))))]
    ['num/ (check-types args env sto 'num 'num 
                        (some (VObject 'num (some (MetaNum 
                                                    (/ (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (make-hash empty))))]
    ['num// (check-types args env sto 'num 'num 
                        (some (VObject 'num (some (MetaNum 
                                                    (quotient (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (make-hash empty))))]
    ['num% (check-types args env sto 'num 'num 
                        (some (VObject 'num (some (MetaNum 
                                                    (quotient (MetaNum-n mval1) 
                                                       (MetaNum-n mval2))))
                                        (make-hash empty))))]
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
                                           (make-hash empty)))
                            (if (> (MetaNum-n mval1) (MetaNum-n mval2))
                                (some (VObject 'num
                                               (some (MetaNum 1))
                                               (make-hash empty)))
                                (some (VObject 'num
                                               (some (MetaNum 0))
                                               (make-hash empty))))))]
    
    ['num-str (let ([arg (first args)])
            (some (VObject 'str 
                           (some (MetaStr 
                             (number->string (MetaNum-n (some-v (VObject-mval
                                                                  arg))))))
                           (make-hash empty))))]
    ;string
    ['str+ (str+ args env sto)]
    ['str= (streq args env sto)]
    ['str* (str* args env sto)]
    ['strcmp (strcmp args env sto)]
    ['strlen (strlen args env sto)]
    ['strbool (strbool args env sto)]
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
    ['list-cpy (list-cpy args env sto)]
    ['list-set (list-set args env sto)]
    ['list-tuple (list-tuple args env sto)]
    ['list-append (list-append args env sto)]

    ;tuple
    ['tuple+ (tuple+ args env sto)]
    ['tuple* (tuple* args env sto)]
    ['tuple-len (tuple-len args env sto)]
    ['tuple-in (tuple-in args env sto)]
    ['tuple-getitem (tuple-getitem args env sto)]
    ['tuple-str (tuple-str args env sto)]
    ['tuple-list (tuple-list args env sto)]
    ['tuple-tuple (tuple-tuple args env sto)]

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

    ;set
    ['set-set (set-set args env sto)]
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
                                (pretty-exception arg sto)))
                        (hash empty))))]
                            

    ;bool
    ['bool-init (bool-init args env sto)]

    ;isinstance
    ['isinstance 
               (if (or (none? (VObject-mval (second args)))
                       (not (MetaClass? (some-v (VObject-mval (second args))))))
               (none)
               (if (object-is? (first args) 
                               (MetaClass-c 
                                 (some-v 
                                   (VObject-mval (second args))))
                               env
                               sto)
                 (some true-val)
                 (some false-val)))]

    ; Returns the class of the given object
    ; If it is an object (i.e., an instance), it is its antecedent.
    ; Otherwise, it is itself.
    ['$class
     (local [(define me (first args))
            (define my-antecedent (VObject-antecedent me))
            (define antecedent-class (fetch (some-v (lookup my-antecedent env)) sto))
            (define am-class (and (some? (VObject-mval me))
                             (MetaClass? (some-v (VObject-mval me)))))]
       (some
         (if am-class
             me
             antecedent-class)))]

    ['$super
     (letrec ([me (first args)]
              [my-antecedent (VObject-antecedent me)]
              [antecedent-class (fetch (some-v (lookup my-antecedent env)) sto)])
       (some antecedent-class))]

    ['$locals (begin
               ; (display env) (display "\n\n")
                (some (make-under-dict (first env) sto)))]

))
