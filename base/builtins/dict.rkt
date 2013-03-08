#lang plai-typed/untyped

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


(define (make-under-dict [h : (hashof symbol Address)] [env : Env] [sto : Store]) : CVal
  (local [(define filledhash (make-hash empty))
          (define dicthash (map (λ (pair)
                                   (hash-set! filledhash
                                              (make-str-value (symbol->string (car pair)))
                                              (fetch (cdr pair) sto)))
                                (hash->list h)))]
  (VObjectClass 'dict
           (some (MetaDict filledhash))
           (hash empty)
           (some (fetch-once (some-v (lookup '%dict env)) sto)))))


(define (dict-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaDict?
               (some (VObjectClass 'int
                              (some (MetaNum (length (hash-keys (MetaDict-contents mval1)))))
                              (hash empty)
                              (some (second args))))))

(define (dict-str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaDict?
               (some (VObjectClass 'str 
                        (some (MetaStr
                                (pretty-metaval mval1)))
                        (hash empty)
                        (some (second args))))))

(define (dict-clear (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaDict?
               (let ([contents (MetaDict-contents mval1)])
                 (begin
                   ; remove all key-value pairs from hash
                   (map (lambda (key) (hash-remove! contents key))
                        (hash-keys contents))
                   (some vnone)))))

(define (dict-in [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaDict?
               (let ([contents (MetaDict-contents mval1)])
                 (if (hash-has-key? contents (second args)) ; FIXME: what if (second args) DNE?
                     (some true-val)
                     (some false-val)))))

(define (dict-get [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaDict?
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
                 (none))))))

(define (dict-update (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaDict? MetaDict?
                    (let ([target (MetaDict-contents mval1)]
                          [extras (MetaDict-contents mval2)])
                      (begin
                        ;; TODO(Sumner): Since the contents of a MetaDict are now VPointers
                        ;; even if they point to the same thing the racket hash doesn't know
                        ;; and adds new mappings. Fix this!
                        (map (lambda (pair)
                               (hash-set! target (car pair) (cdr pair)))
                             (hash->list extras))
                        (some vnone)))))

(define (dict-keys (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaDict?
               (let ([contents (MetaDict-contents mval1)])
                    (some
                      (VObjectClass 'set
                               (some (MetaSet (make-set (hash-keys contents))))
                               (hash empty)
                               (some (second args)))))))

(define (dict-values (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaDict?
               (let ([contents (MetaDict-contents mval1)])
                    (some
                      (VObjectClass 'set
                               (some (MetaSet (make-set (hash-values contents))))
                               (hash empty)
                               (some (second args)))))))

(define (dict-items (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaDict?
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
  (check-types-pred args env sto MetaDict?
               (letrec ([contents (MetaDict-contents mval1)]
                        [target (second args)])
                 (hash-ref contents target))))

(define (dict-setitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaDict?
               (letrec ([contents (MetaDict-contents mval1)]
                        [target (second args)]
                        [value (third args)])
                 (begin
                   (hash-set! contents target value)
                   (some vnone)))))

(define (dict-delitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaDict?
               (letrec ([contents (MetaDict-contents mval1)]
                        [target (second args)])
                 (begin
                   (hash-remove! contents target)
                   (some vnone)))))

(define (dict->list [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaDict?
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
              ;; preserve obj class pointer when set to support inheritance
              (if (some? (VObjectClass-class obj))
                  (VObjectClass-class obj)
                  (some (second args)))))))

