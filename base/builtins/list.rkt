#lang plai-typed

(require "../python-core-syntax.rkt"
         "../util.rkt"
         "num.rkt"
         "none.rkt")

(define (make-builtin-list [l : (listof CVal)] [class : CVal]) : CVal
  (VObjectClass 'list
           (some (MetaList l))
           (hash empty)
           (some class)))

(define (list+ (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'list 'list
               (some (VObjectClass 'list
                              (some (MetaList
                                     (append (MetaList-v mval1)
                                             (MetaList-v mval2))))
                              (hash empty)
                              (some (third args))))))

(define (list-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'list
               (some (VObjectClass 'num
                              (some (MetaNum (length (MetaList-v mval1))))
                              (hash empty)
                              (some (second args))))))

(define (list-copy [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'list
         (some (VObjectClass 'list (some (MetaList (MetaList-v mval1)))
                        (hash empty)
                        (some (second args))))))


(define (list-tuple [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'list
         (some (VObjectClass 'tuple (some (MetaTuple (MetaList-v mval1)))
                        (hash empty)
                        (some (second args))))))

(define (list-in [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
 (letrec ([self-list (MetaList-v (some-v (VObjectClass-mval (first args))))]
          [test (second args)]
          [contains (lambda ([lst : (listof CVal)] [val : CVal]) : CVal
                    (cond
                     [(empty? lst) false-val]
                     [(cons? lst)
                       (if (equal? val (first lst))
                         true-val
                         (contains (rest lst) val))]))])
   (some (contains self-list test))))

(define (list-getitem (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  ; here we'll eventually need to support slicin' and dicin' bro
  (check-types args env sto 'list 'num
               (some 
                 (try
                   (list-ref (MetaList-v mval1) (MetaNum-n mval2))
                   (lambda ()
                     vnone)))))

(define (list-str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'list
               (some (VObjectClass 'str 
                        (some (MetaStr
                                (pretty-metaval mval1)))
                        (hash empty)
                        (some (second args))))))

(define (list-set (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'list
               (let ([values (MetaList-v mval1)])
                    (some (VObjectClass 'set
                                   (some (MetaSet (make-set values)))
                                   (hash empty)
                                   (some (second args)))))))

(define (list-setitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'list 'num
               (some (make-builtin-list
                       (list-replace (MetaNum-n mval2) 
                                     (third args)
                                     (MetaList-v mval1))
                       (fourth args)))))

