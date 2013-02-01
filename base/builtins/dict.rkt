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

(define dict-class : CExpr
  (seq-ops (list 
             (CAssign (CId '$dict (GlobalId))
                      (CClass
                        '$dict
                        (list 'object)
                        (CNone)))
             (def '$dict '__len__
                  (CFunc (list 'self) (none)
                         (CReturn (CBuiltinPrim 'dict-len
                                                (list
                                                  (CId 'self (LocalId)))))
                         true))
             (def '$dict '__str__
                  (CFunc (list 'self) (none)
                         (CReturn (CBuiltinPrim 'dict-str
                                                (list (CId 'self (LocalId)))))
                         true))

             (def '$dict '__list__
                  (CFunc (list 'self) (none)
                         (CReturn (CBuiltinPrim 'dict->list
                                                (list (CId 'self (LocalId)))))
                         true))

             (def '$dict 'clear
                  (CFunc (list 'self) (none)
                         (CReturn (CBuiltinPrim 'dict-clear
                                                (list (CId 'self (LocalId)))))
                         true))

             (def '$dict 'update
                  (CFunc (list 'self) (some 'other)
                         (CReturn (CBuiltinPrim 'dict-update
                                                (list (CId 'self (LocalId))
                                                      (CId 'other (LocalId)))))
                         true))
             (def '$dict 'get
                  (CFunc (list 'self 'key) (some 'default)
                         (CReturn (CBuiltinPrim 'dict-get 
                                                (list (CId 'self (LocalId)) 
                                                      (CId 'key (LocalId)) 
                                                      (CId 'default (LocalId)))))
                         true))
             (def '$dict '__iter__
                  (local [(define keys-id (new-id))]
                    (CFunc (list 'self) (none)
                           (CLet keys-id (LocalId)
                                 (CApp (CGetField (CId 'self (LocalId)) 'keys) 
                                       (list (CId 'self (LocalId)))
                                       (none))
                                 (CReturn (CApp (CGetField (CId keys-id (LocalId)) '__iter__)
                                                (list (CId keys-id (LocalId)))
                                                (none))))
                           true)))
             (def '$dict '__in__
                  (CFunc (list 'self 'other) (none)
                         (CReturn (CBuiltinPrim 'dict-in
                                                (list
                                                  (CId 'self (LocalId))
                                                  (CId 'other (LocalId)))))
                         true))

             (def '$dict '__eq__
                  (CFunc (list 'self 'other) (none)
                         (CReturn (CApp (CId 'dicteq (GlobalId))
                                        (list
                                          (CId 'self (LocalId))
                                          (CId 'other (LocalId)))
                                        (none)))
                         true))

             (def '$dict 'keys
                  (CFunc (list 'self) (none)
                         (CReturn (CBuiltinPrim 'dict-keys
                                                (list (CId 'self (LocalId)))))
                         true))

             (def '$dict 'values
                  (CFunc (list 'self) (none)
                         (CReturn (CBuiltinPrim 'dict-values
                                                (list (CId 'self (LocalId)))))
                         true))

             (def '$dict 'items
                  (CFunc (list 'self) (none)
                         (CReturn (CBuiltinPrim 'dict-items
                                                (list (CId 'self (LocalId)))))
                         true))

             (def '$dict '__getitem__
                  (CFunc (list 'self 'other) (none)
                         (CReturn (CBuiltinPrim 'dict-getitem
                                                (list (CId 'self (LocalId))
                                                      (CId 'other (LocalId)))))
                         true))

             (def '$dict '__setitem__
                  (local ((define loc-id (new-id)))
                         (CFunc (list 'self 'target 'value) (none)
                                (CSeq
                                 (CLet loc-id (LocalId)
                                       (CPrim2 'find-addr
                                               (CId 'self (LocalId))
                                               (CId 'target
                                                    (LocalId)))
                                       (CPrim2 'set-store
                                               (CId loc-id (LocalId))
                                               (CId 'value (LocalId))))
                                 (CReturn (CNone)))
                                true)))

             (def '$dict '__delitem__
                  (CFunc (list 'self 'slice) (none)
                         (CReturn (CPrim2 'dict-delitem
                                          (CId 'self (LocalId))
                                          (CId 'slice (LocalId))))
                         true)))))


(define (make-under-dict [h : (hashof symbol Address)] [sto : Store]) : CVal
  (local [(define filledhash (make-hash empty))
          (define dicthash (map (λ (pair)
                                   (hash-set! filledhash
                                              (make-str-value (symbol->string (car pair)))
                                              (cdr pair)))
                                (hash->list h)))]
  (VObject '$dict
           (some (MetaDict filledhash))
           (hash empty))))


(define (dict-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$dict
               (some (VObject 'num
                              (some (MetaNum (length (hash-keys (MetaDict-contents mval1)))))
                              (hash empty)))))

(define (dict-str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$dict
               (some (VObject 'str 
                        (some (MetaStr
                                (pretty-metaval mval1 sto)))
                        (hash empty)))))

(define (dict-clear (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$dict
               (let ([contents (MetaDict-contents mval1)])
                 (begin
                   ; remove all key-value pairs from hash
                   (map (lambda (key) (hash-remove! contents key))
                        (hash-keys contents))
                   (some vnone)))))

(define (dict-in [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$dict
               (let ([contents (MetaDict-contents mval1)])
                 (if (hash-has-key? contents (second args)) ; FIXME: what if (second args) DNE?
                     (some true-val)
                     (some false-val)))))
(define (dict-get [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$dict
      (local [(define d (first args))
              (define meta-d (MetaDict-contents (some-v (VObject-mval d))))
              (define key (second args))
              (define startuple (third args))
              (define meta-startuple (MetaTuple-v (some-v (VObject-mval
                                                            startuple))))
              (define mayb-val (hash-ref meta-d key))]
             (if (some? mayb-val)
                 (some (fetch (some-v mayb-val) sto))
               (if (not (= 0 (length meta-startuple)))
                 (some (first meta-startuple))
                 (some vnone))))))

(define (dict-update (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$dict
     (let ([starargs (MetaTuple-v (some-v (VObject-mval (second args))))])
        (if (= 1 (length starargs))
            (let ([target (MetaDict-contents mval1)]
                  [extras (MetaDict-contents (some-v (VObject-mval (first starargs))))])
                 (begin
                   (map (lambda (pair)
                          (hash-set! target (car pair) (cdr pair)))
                        (hash->list extras))
                   (some vnone)))
            (some vnone)))))

(define (dict-keys (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$dict
               (let ([contents (MetaDict-contents mval1)])
                    (some
                      (VObject 'set
                               (some (MetaSet (make-set (hash-keys contents))))
                               (hash empty))))))

(define (dict-values (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$dict
               (let* ([contents (MetaDict-contents mval1)]
                      ;fetch values from store based on the address
                      [vals (map (lambda (addr)
                                   (fetch addr sto))
                                 (hash-values contents))])
                    (some
                      (VObject 'set
                               (some (MetaSet (make-set vals)))
                               (hash empty))))))

(define (dict-items (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$dict
               (letrec ([contents (MetaDict-contents mval1)]
                        [items (map (lambda (pair) ; create a tuple for each (key, value)
                                            (VObject 'tuple
                                                     (some (MetaTuple (list (car pair)
                                                                            (fetch (cdr pair) sto))))
                                                     (hash empty)))
                                    (hash->list contents))])
                    (some
                      (VObject 'set
                               (some (MetaSet (make-set items)))
                               (hash empty))))))


(define (dict-getitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$dict
               (letrec ([contents (MetaDict-contents mval1)]
                        [target (second args)]
                        [mayb-val (hash-ref contents target)])
                 (if (some? mayb-val)
                   (some (fetch (some-v mayb-val) sto))
                   (some vnone)))))

;; (define (dict-setitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
;;   (check-types args env sto '$dict
;;                (letrec ([contents (MetaDict-contents mval1)]
;;                         [target (second args)]
;;                         [value (third args)])
;;                  (begin
;;                    (hash-set! contents target value)
;;                    (some vnone)))))

;; (define (dict-delitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
;;   (check-types args env sto '$dict
;;                (letrec ([contents (MetaDict-contents mval1)]
;;                         [target (second args)])
;;                  (begin
;;                    (hash-remove! contents target)
;;                    (some vnone)))))

(define (dict->list [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
     (check-types args env sto '$dict
                  (local [(define contents (MetaDict-contents mval1))]
                    (some
                      (VObject 'list
                               (some (MetaList (hash-keys contents)))
                               (hash empty))))))
