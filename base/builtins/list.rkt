#lang plai-typed

(require "../python-core-syntax.rkt"
         "../util.rkt"
         "num.rkt"
         "none.rkt")

(define list-class : CExpr
  (seq-ops (list
             (CAssign (CId 'list (GlobalId))
                      (CClass
                        'list
                        (list 'object)
                        (CNone)))
             (def 'list '__add__
                  (CFunc (list 'self 'other) (none)
                         (CReturn (CBuiltinPrim 'list+
                                                (list
                                                  (CId 'self (LocalId))
                                                  (CId 'other (LocalId)))))
                         true))

             (def 'list 'extend
                  (CFunc (list 'self 'other) (none)
                         (CAssign (CId 'self (LocalId))
                                  (CBuiltinPrim 'list+
                                                (list
                                                  (CId 'self (LocalId))
                                                  (CId 'other (LocalId)))))
                         true))
             (def 'list '__init__
                  (CFunc (list 'self 'other) (none) 
                         (CAssign (CId 'self (LocalId))
                                  (CApp (CGetField (CId 'other (LocalId)) '__list__)
                                        (list (CId 'other (LocalId)))
                                        (none)))
                         true))
             (def 'list '__len__
                  (CFunc (list 'self) (none)
                         (CReturn (CBuiltinPrim 'list-len
                                                (list
                                                  (CId 'self (LocalId)))))
                         true))
             (def 'list '__list__
                  (CFunc (list 'self) (none)
                         (CReturn (CBuiltinPrim 'list-cpy
                                                (list 
                                                  (CId 'self (LocalId)))))
                         true))
             (def 'list '__iter__
                  (CFunc (list 'self) (none)
                         (CReturn (CApp (CGetField (CId 'SeqIter (LocalId)) '__init__)
                                        (list (CObject 'SeqIter (none)) 
                                              (CId 'self (LocalId)))
                                        (none)))
                         true))

             (def 'list '__tuple__
                  (CFunc (list 'self) (none)
                         (CReturn (CBuiltinPrim 'list-tuple
                                                (list (CId 'self (LocalId)))))
                         true))

             (def 'list '__set__
                  (CFunc (list 'self) (none)
                         (CReturn (CBuiltinPrim 'list-set
                                                (list
                                                  (CId 'self (LocalId)))))
                         true))
             (def 'list '__in__
                  (CFunc (list 'self 'test) (none)
                         (CReturn (CBuiltinPrim 'list-in
                                                (list
                                                  (CId 'self (LocalId))
                                                  (CId 'test (LocalId)))))
                         true))
             (def 'list '__str__
                  (CFunc (list 'self) (none)
                         (CReturn (CBuiltinPrim 'list-str
                                                (list (CId 'self (LocalId)))))
                         true))
             (def 'list '__getitem__
                  (CFunc (list 'self 'idx) (none)
                         (CReturn (CBuiltinPrim 'list-getitem
                                                (list
                                                  (CId 'self (LocalId))
                                                  (CId 'idx (LocalId)))))
                         true))
             (def 'list '__setitem__
                  (CFunc (list 'self 'idx 'val) (none)
                         (CAssign (CId 'self (LocalId))
                                  (CBuiltinPrim 'list-setitem
                                                (list
                                                  (CId 'self (LocalId))
                                                  (CId 'idx (LocalId))
                                                  (CId 'val (LocalId)))))
                         true))
             (def 'list '__cmp__
                  (CFunc (list 'self 'other) (none)
                    (CLet 'listcmp (LocalId)
                      (CFunc (list 'self 'other 'idx) (none)
                        (CLet 'li1 (LocalId)
                          (CApp (CGetField (CId 'self (LocalId))
                                           '__getitem__)
                                (list (CId 'self (LocalId))
                                      (CId 'idx (LocalId)))
                                (none))
                          (CLet 'li2 (LocalId)
                                (CApp (CGetField (CId 'other (LocalId))
                                                 '__getitem__)
                                      (list (CId 'other (LocalId))
                                            (CId 'idx (LocalId)))
                                      (none))
                                (CIf (CPrim2 'Is (CId 'li1 (LocalId)) (CNone))
                                     (CIf (CPrim2 'Is (CId 'li2 (LocalId)) (CNone))
                                          (CReturn (make-builtin-num 0))
                                          (CReturn (make-builtin-num -1)))
                                     (CIf (CPrim2 'Is (CId 'li2 (LocalId)) (CNone))
                                          (CReturn (make-builtin-num 1))
                                          (CLet 'cmpval (LocalId)
                                                (CApp (CGetField (CId 'li1 (LocalId))
                                                                 '__cmp__)
                                                      (list (CId 'li1 (LocalId))
                                                            (CId 'li2 (LocalId)))
                                                      (none))
                                                (CIf (CApp (CGetField (CId 'cmpval (LocalId))
                                                                      '__eq__)
                                                           (list (CId 'cmpval (LocalId))
                                                                 (make-builtin-num 0))
                                                           (none))
                                                     (CLet 'nidx (LocalId)
                                                           (CApp (CGetField (CId 'idx (LocalId))
                                                                            '__add__)
                                                                 (list (CId 'idx (LocalId))
                                                                       (make-builtin-num 1))
                                                                 (none))
                                                           (CReturn 
                                                             (CApp (CId 'listcmp (LocalId))
                                                                   (list (CId 'self (LocalId))
                                                                         (CId 'other (LocalId))
                                                                         (CId 'nidx (LocalId)))
                                                                   (none))))
                                                     (CReturn (CId 'cmpval (LocalId)))))))))
                        false)
                      (CReturn 
                        (CApp (CId 'listcmp (LocalId))
                              (list (CId 'self (LocalId))
                                    (CId 'other (LocalId))
                                    (make-builtin-num 0))
                              (none))))
                    true))

             (def 'list 'append
                  (CFunc (list 'self 'other) (none)
                         (CAssign (CId 'self (LocalId))
                                  (CBuiltinPrim 'list-append 
                                                (list (CId 'self (LocalId)) 
                                                      (CId 'other (LocalId)))))
                         true))
             (def 'list '__eq__
                  (CFunc (list 'self 'other) (none)
                         (CLet '_cmpresult (LocalId)
                               (CApp (CGetField (CId 'self (LocalId)) '__cmp__)
                                     (list (CId 'self (LocalId))
                                           (CId 'other (LocalId)))
                                     (none))
                               (CReturn (CApp (CGetField (CId '_cmpresult (LocalId))
                                                         '__eq__)
                                              (list (CId '_cmpresult (LocalId))
                                                    (make-builtin-num 0))
                                              (none))))
                         true)))))

(define (make-builtin-list [l : (listof CVal)]) : CVal
  (VObject 'list
           (some (MetaList l))
           (hash empty)))

(define (list+ (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'list 'list
               (some (VObject 'list
                              (some (MetaList
                                     (append (MetaList-v mval1)
                                             (MetaList-v mval2))))
                              (hash empty)))))

(define (list-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'list
               (some (VObject 'num
                              (some (MetaNum (length (MetaList-v mval1))))
                              (hash empty)))))
(define (list-cpy [args : (listof CVal)]
                  [env : Env]
                  [sto : Store]) : (optionof CVal)
  (check-types args env sto 'list
         (some (make-builtin-list (MetaList-v mval1)))))

(define (list-append [args : (listof CVal)]
                  [env : Env]
                  [sto : Store]) : (optionof CVal)
  (check-types args env sto 'list
         (some (make-builtin-list (append (MetaList-v mval1)
                                          (list (second args)))))))

(define (list-tuple [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'list
         (some (VObject 'tuple (some (MetaTuple (MetaList-v mval1))) (hash empty)))))

(define (list-in [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
 (letrec ([self-list (MetaList-v (some-v (VObject-mval (first args))))]
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
               (some (VObject 'str 
                        (some (MetaStr
                                (pretty-metaval mval1)))
                        (hash empty)))))

(define (list-set (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'list
               (let ([values (MetaList-v mval1)])
                    (some (VObject 'set
                                   (some (MetaSet (make-set values)))
                                   (hash empty))))))
(define (list-setitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal) 
  (check-types args env sto 'list 'num 'num
               (some (make-builtin-list
                       (list-replace (MetaNum-n mval2) 
                                     (third args)
                                     (MetaList-v mval1))))))
