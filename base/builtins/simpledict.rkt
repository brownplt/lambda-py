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

(define simpledict-class : CExpr
  (CClass
   '$simpledict
   (list 'object)
   (seq-ops (list 
             (def '__init__
                       (CFunc (list 'self) (none)
                              (CAssign
                                (CId 'self (LocalId))
                                (CBuiltinPrim 'simpledict-init (list)))
                              true))
             
              (def '__len__
                    (CFunc (list 'self) (none)
                           (CReturn (CBuiltinPrim 'simpledict-len
                                                  (list
                                                   (CId 'self (LocalId)))))
                           true))
              (def '__str__
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'simpledict-str
                                                     (list (CId 'self (LocalId)))))
                          true))

              (def 'get
                   (CFunc (list 'self 'key) (some 'default)
                          (CReturn (CBuiltinPrim 'simpledict-get 
                                        (list (CId 'self (LocalId)) 
                                              (CId 'key (LocalId)) 
                                              (CId 'default (LocalId)))))
                          true))
              (def '__in__
                (CFunc (list 'self 'other) (none)
                       (CReturn (CBuiltinPrim 'simpledict-in
                                              (list
                                               (CId 'self (LocalId))
                                               (CId 'other (LocalId)))))
                       true))

              (def '__getitem__
                   (CFunc (list 'self 'other) (none)
                          (CReturn (CBuiltinPrim 'simpledict-getitem
                                                     (list (CId 'self (LocalId))
                                                           (CId 'other (LocalId)))))
                          true))

              (def '__setitem__
                   (CFunc (list 'self 'target 'value) (none)
                          (CSeq
                           (CLet 'loc (CPrim2 'simpledict-find (CId 'self (LocalId)) (CId 'target (LocalId)))
                                 (CPrim2 'set-store (CId 'loc (LocalId)) (CId 'value (LocalId))))
                           (CReturn (CNone)))
                          true))
              
              (def 'bind
                (CFunc (list 'self 'target) (none)
                       (CReturn (CPrim2 'simpledict-bind 
                                        (CId 'self (LocalId))
                                        (CId 'target (LocalId))))
                          true))
))))

(define (simpledict-init (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (some (VObject '$simpledict 
                 (some (MetaSimpleDict (hash empty)))
                 (make-hash empty))))

(define (simpledict-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$simpledict
               (some (VObject 'num
                              (some (MetaNum (length (hash-keys (MetaSimpleDict-contents mval1)))))
                              (hash empty)))))

(define (simpledict-str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$simpledict
               (some (VObject 'str 
                        (some (MetaStr
                                (pretty-metaval mval1)))
                        (make-hash empty)))))

(define (simpledict-in [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$simpledict 'str
               (let ([contents (MetaSimpleDict-contents mval1)]
                     [key (string->symbol (MetaStr-s mval2))])
                 (if (hash-has-key? contents key) ; FIXME: what if (second args) DNE?
                     (some true-val)
                     (some false-val)))))

(define (simpledict-get [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$simpledict 'str
      (local [(define contents (MetaSimpleDict-contents mval1))
              (define key (string->symbol (MetaStr-s mval2)))
              (define startuple (third args))
              (define meta-startuple (MetaTuple-v (some-v (VObject-mval
                                                            startuple))))
              (define mayb-loc (hash-ref contents key))]
             (some
              (if (some? mayb-loc)
                  (fetch (some-v mayb-loc) sto)
                  (if (not (= 0 (length meta-startuple)))
                      (first meta-startuple)
                      vnone))))))

(define (simpledict-getitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$simpledict 'str
               (letrec ([contents (MetaSimpleDict-contents mval1)]
                        [key (string->symbol (MetaStr-s mval2))]
                        [mayb-loc (hash-ref contents key)])
                 (some
                  (if (some? mayb-loc)
                      (fetch (some-v mayb-loc) sto)
                      vnone)))))

