#lang plai-typed/untyped

(require "../python-core-syntax.rkt"
         "../util.rkt"
         "num.rkt"
         (typed-in racket/list (take : ((listof 'a) number -> (listof 'a))))
         (typed-in racket/list (drop : ((listof 'a) number -> (listof 'a)))))

(define (make-builtin-list [l : (listof CVal)] [class : CVal]) : CVal
  (VObjectClass 'list
           (some (MetaList l))
           (hash empty)
           (some class)))

(define (list+ (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaList? MetaList?
               (some (VObjectClass 'list
                              (some (MetaList
                                     (append (MetaList-v mval1)
                                             (MetaList-v mval2))))
                              (hash empty)
                              (some (third args))))))

(define (list-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaList?
               (some (VObjectClass 'int
                              (some (MetaNum (length (MetaList-v mval1))))
                              (hash empty)
                              (some (second args))))))

(define (list-copy [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaList?
         (some (VObjectClass 'list (some (MetaList (MetaList-v mval1)))
                        (hash empty)
                        (some (second args))))))


(define (list-tuple [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaList?
         (some (VObjectClass 'tuple (some (MetaTuple (MetaList-v mval1)))
                        (hash empty)
                        (some (second args))))))

(define (list-getitem (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  ; here we'll eventually need to support slicin' and dicin' bro
  (check-types-pred args env sto MetaList? MetaNum?
                 (try
                   (some (list-ref (MetaList-v mval1) (MetaNum-n mval2)))
                   (lambda ()
                     (none)))))

(define (list-str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaList?
               (some (VObjectClass 'str 
                        (some (MetaStr
                                (pretty-metaval mval1 sto)))
                        (hash empty)
                        (some (second args))))))

(define (list-set (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaList?
               (let ([values (MetaList-v mval1)])
                    (some (VObjectClass 'set
                                   (some (MetaSet (make-set values)))
                                   (hash empty)
                                   (some (second args)))))))

(define (list-setitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaList? MetaNum?
               (some (make-builtin-list
                       (list-replace (MetaNum-n mval2) 
                                     (third args)
                                     (MetaList-v mval1))
                       (fourth args)))))

(define (list-remove [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaList? MetaNum?
               (some (make-builtin-list
                       (append (take (MetaList-v mval1)
                                     (MetaNum-n mval2))
                               (drop (MetaList-v mval1)
                                     (add1 (MetaNum-n mval2))))
                       (third args)))))


(define (list-init [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (let ([obj (first args)])
    (some
     (VObjectClass (VObjectClass-antecedent obj)
              (some (MetaList empty))
              (VObjectClass-dict obj)
              ;; preserve obj class pointer when set to support inheritance
                  (some (second args))))))
