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
  (CClass
   '$dict
   'object
   (seq-ops (list 
              (def '__len__
                    (CFunc (list 'self) (none)
                           (CReturn (CBuiltinPrim 'dict-len
                                                  (list
                                                   (CId 'self (LocalId)))))
                           true))
              (def '__str__
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'dict-str
                                                     (list (CId 'self (LocalId)))))
                          true))

              (def '__list__
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'dict->list
                                                 (list (CId 'self (LocalId)))))
                          true))

              (def 'clear
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'dict-clear
                                                     (list (CId 'self (LocalId)))))
                          true))

              (def 'update
                   (CFunc (list 'self) (some 'other)
                          (CReturn (CBuiltinPrim 'dict-update
                                                     (list (CId 'self (LocalId))
                                                           (CId 'other (LocalId)))))
                          true))
              (def 'get
                   (CFunc (list 'self 'key) (some 'default)
                          (CReturn (CBuiltinPrim 'dict-get 
                                        (list (CId 'self (LocalId)) 
                                              (CId 'key (LocalId)) 
                                              (CId 'default (LocalId)))))
                          true))
              (def '__iter__
                   (let ([keys-id (CId (new-id) (LocalId))])
                     (CFunc (list 'self) (none)
                        (CSeq (CAssign keys-id 
                                       (CApp (CGetField (CId 'self (LocalId)) 'keys) 
                                             (list (CId 'self (LocalId)))
                                             (none)))
                              (CReturn (CApp (CGetField keys-id '__iter__)
                                    (list keys-id)
                                    (none))))
                        true)))
              (def '__in__
                (CFunc (list 'self 'other) (none)
                       (CReturn (CBuiltinPrim 'dict-in
                                              (list
                                               (CId 'self (LocalId))
                                               (CId 'other (LocalId)))))
                       true))

              (def '__eq__
                (CFunc (list 'self 'other) (none)
                       (CReturn (CApp (CId 'dicteq (GlobalId))
                                      (list
                                        (CId 'self (LocalId))
                                        (CId 'other (LocalId)))
                                      (none)))
                       true))

              (def 'keys
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'dict-keys
                                                     (list (CId 'self (LocalId)))))
                          true))

              (def 'values
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'dict-values
                                                     (list (CId 'self (LocalId)))))
                          true))
              
              (def 'items
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'dict-items
                                                     (list (CId 'self (LocalId)))))
                          true))

              (def '__getitem__
                   (CFunc (list 'self 'other) (none)
                          (CReturn (CBuiltinPrim 'dict-getitem
                                                     (list (CId 'self (LocalId))
                                                           (CId 'other (LocalId)))))
                          true))

              (def '__setitem__
                   (CFunc (list 'self 'target 'value) (none)
                          (CReturn (CBuiltinPrim 'dict-setitem
                                                     (list (CId 'self (LocalId))
                                                           (CId 'target (LocalId))
                                                           (CId 'value (LocalId)))))
                          true))

              (def '__delitem__
                   (CFunc (list 'self 'slice) (none)
                          (CReturn (CBuiltinPrim 'dict-delitem
                                                     (list (CId 'self (LocalId))
                                                           (CId 'slice (LocalId)))))
                          true))

))))


(define (make-under-dict [h : (hashof symbol Address)] [sto : Store]) : CVal
  (local [(define filledhash (make-hash empty))
          (define dicthash (map (λ (pair)
                                   (hash-set! filledhash
                                              (make-str-value (symbol->string (car pair)))
                                              (fetch (cdr pair) sto)))
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
                                (pretty-metaval mval1)))
                        (make-hash empty)))))

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
               mayb-val
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
                               (make-hash empty))))))

(define (dict-values (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$dict
               (let ([contents (MetaDict-contents mval1)])
                    (some
                      (VObject 'set
                               (some (MetaSet (make-set (hash-values contents))))
                               (make-hash empty))))))

(define (dict-items (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$dict
               (letrec ([contents (MetaDict-contents mval1)]
                        [items (map (lambda (pair) ; create a tuple for each (key, value)
                                            (VObject 'tuple
                                                     (some (MetaTuple (list (car pair) (cdr pair))))
                                                     (make-hash empty)))
                                    (hash->list contents))])
                    (some
                      (VObject 'set
                               (some (MetaSet (make-set items)))
                               (make-hash empty))))))


(define (dict-getitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$dict
               (letrec ([contents (MetaDict-contents mval1)]
                        [target (second args)]
                        [mayb-val (hash-ref contents target)])
                 (if (some? mayb-val)
                   mayb-val
                   (some vnone)))))

(define (dict-setitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$dict
               (letrec ([contents (MetaDict-contents mval1)]
                        [target (second args)]
                        [value (third args)])
                 (begin
                   (hash-set! contents target value)
                   (some vnone)))))

(define (dict-delitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$dict
               (letrec ([contents (MetaDict-contents mval1)]
                        [target (second args)])
                 (begin
                   (hash-remove! contents target)
                   (some vnone)))))

(define (dict->list [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
     (check-types args env sto '$dict
                  (local [(define contents (MetaDict-contents mval1))]
                    (some
                      (VObject 'list
                               (some (MetaList (hash-keys contents)))
                               (make-hash empty))))))
