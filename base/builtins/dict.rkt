#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt"
         "none.rkt"
         "set.rkt"
         "str.rkt")
(require
  (typed-in racket/base (andmap : (('a -> boolean) (listof 'a) -> 'b)))
  (typed-in racket/base (hash->list : ((hashof 'a 'b)  -> (listof 'c))))
  (typed-in racket/base (car : (('a * 'b)  -> 'a)))
  (typed-in racket/base (cdr : (('a * 'b)  -> 'b)))
  (typed-in racket/base (hash-has-key? : ((hashof 'a 'b) 'a -> boolean)))
  (typed-in racket/base (hash-values : ((hashof 'a 'b) -> (listof 'b))))
)


(define (make-under-dict [h : (hashof symbol Address)] [sto : Store]) : CVal
  (local [(define filledhash (make-hash empty))
          (define dicthash (map (λ (pair)
                                   (hash-set! filledhash
                                              (make-str-value (symbol->string (car pair)))
                                              (fetch (cdr pair) sto)))
                                (hash->list h)))]
  (VObject 'dict
           (some (MetaDict filledhash))
           (hash empty))))


(define (dict-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (some (VObjectClass 'num
                              (some (MetaNum (length (hash-keys (MetaDict-contents mval1)))))
                              (hash empty)
                              (some (second args))))))

(define (dict-str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (some (VObjectClass 'str 
                        (some (MetaStr
                                (pretty-metaval mval1)))
                        (hash empty)
                        (some (second args))))))

(define (dict-clear (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (let ([contents (MetaDict-contents mval1)])
                 (begin
                   ; remove all key-value pairs from hash
                   (map (lambda (key) (hash-remove! contents key))
                        (hash-keys contents))
                   (some vnone)))))

(define (dict-in [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (let ([contents (MetaDict-contents mval1)])
                 (if (hash-has-key? contents (second args)) ; FIXME: what if (second args) DNE?
                     (some true-val)
                     (some false-val)))))

(define (dict-get [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
      (local [(define d (first args))
              (define meta-d (MetaDict-contents (some-v (VObjectClass-mval d))))
              (define key (second args))
              (define startuple (third args))
              (define meta-startuple (MetaTuple-v (some-v (VObjectClass-mval
                                                            startuple))))
              (define mayb-val (hash-ref meta-d key))]
             (if (some? mayb-val)
               mayb-val
               (if (not (= 0 (length meta-startuple)))
                 (some (first meta-startuple))
                 (some vnone))))))

(define (dict-update (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (let ([starargs (MetaTuple-v (some-v (VObjectClass-mval (second args))))])
                 (cond
                  ;; only work when the argument is a dict, it should handle pair iterators.
                  [(and (= 1 (length starargs)) 
                        (VObjectClass? (first starargs)) 
                        (some? (VObjectClass-mval (first starargs)))
                        (MetaDict? (some-v (VObjectClass-mval (first starargs)))))
                   (let ([target (MetaDict-contents mval1)]
                         [extras (MetaDict-contents (some-v (VObjectClass-mval (first starargs))))])
                     (begin
                       (map (lambda (pair)
                              (hash-set! target (car pair) (cdr pair)))
                            (hash->list extras))
                       (some vnone)))]
                  [(= 0 (length starargs))
                   (some vnone)]
                  [else
                   (none)]))))

(define (dict-keys (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (let ([contents (MetaDict-contents mval1)])
                    (some
                      (VObjectClass 'set
                               (some (MetaSet (make-set (hash-keys contents))))
                               (hash empty)
                               (some (second args)))))))

(define (dict-values (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (let ([contents (MetaDict-contents mval1)])
                    (some
                      (VObjectClass 'set
                               (some (MetaSet (make-set (hash-values contents))))
                               (hash empty)
                               (some (second args)))))))

(define (dict-items (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (letrec ([contents (MetaDict-contents mval1)]
                        [items (map (lambda (pair) ; create a tuple for each (key, value)
                                            (VObjectClass 'tuple
                                                     (some (MetaTuple (list (car pair) (cdr pair))))
                                                     (hash empty)
                                                     (some (third args))))
                                    (hash->list contents))])
                    (some
                      (VObjectClass 'set
                               (some (MetaSet (make-set items)))
                               (hash empty)
                               (some (second args)))))))


(define (dict-getitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (letrec ([contents (MetaDict-contents mval1)]
                        [target (second args)]
                        [mayb-val (hash-ref contents target)])
                 (if (some? mayb-val)
                   mayb-val
                   (some vnone)))))

(define (dict-setitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (letrec ([contents (MetaDict-contents mval1)]
                        [target (second args)]
                        [value (third args)])
                 (begin
                   (hash-set! contents target value)
                   (some vnone)))))

(define (dict-delitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (letrec ([contents (MetaDict-contents mval1)]
                        [target (second args)])
                 (begin
                   (hash-remove! contents target)
                   (some vnone)))))

(define (dict->list [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'dict
               (local [(define contents (MetaDict-contents mval1))]
                 (some
                   (VObjectClass 'list
                            (some (MetaList (hash-keys contents)))
                            (hash empty)
                            (some (second args)))))))

(define (dict-init [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (let ([obj (first args)])
    (some
     (VObjectClass (VObjectClass-antecedent obj)
              (some (MetaDict (make-hash empty)))
              (VObjectClass-dict obj)
              (some (second args))))))

