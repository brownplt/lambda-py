#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt"
         "none.rkt"
         "num.rkt"
         "list.rkt")

(define tuple-class : CExpr
  (CClass
   'tuple
   (list 'object)
   (seq-ops (list (def '__add__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'tuple+
                                                  (list
                                                   (CId 'self (LocalId))
                                                   (CId 'other (LocalId)))))
                           (some 'tuple)))

                   (def '__init__
                        (CFunc (list 'self) (some 'args)
                          (CReturn
                          (CIf ; Did we get any args?
                            (CBuiltinPrim 'num=
                                          (list
                                            (CApp (CGetField (CId 'args (LocalId)) '__len__)
                                                  (list)
                                                  (none))
                                            (CObject 'num (some (MetaNum 0)))))
                            ; No. Return an empty tuple.
                            (CTuple empty)
                            ; Yes. Call __tuple__ on the first argument.
                            (CLet 'first-arg
                                  (CApp (CGetField (CId 'args (LocalId)) '__getitem__)
                                        (list (CObject 'num (some (MetaNum 0))))
                                        (none))
                                  (CApp (CGetField (CId 'first-arg (LocalId)) '__tuple__)
                                        (list)
                                        (none)))))
                          (some 'tuple)))

                  (def '__mult__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'tuple*
                                                  (list
                                                   (CId 'self (LocalId))
                                                   (CId 'other (LocalId)))))
                           (some 'tuple)))
                  (def '__len__
                    (CFunc (list 'self) (none)
                           (CReturn (CBuiltinPrim 'tuple-len
                                                  (list
                                                   (CId 'self (LocalId)))))
                           (some 'tuple)))
                  (def '__in__
                    (CFunc (list 'self 'test) (none)
                           (CReturn (CBuiltinPrim 'tuple-in
                                                  (list
                                                   (CId 'self (LocalId))
                                                   (CId 'test (LocalId)))))
                           (some 'tuple)))
                  (def '__iter__
                       (CFunc (list 'self) (none)
                           (CReturn (CApp (CId 'SeqIter (LocalId))
                                          (list (CId 'self (LocalId)))
                                          (none)))
                           (some 'tuple)))
                  (def '__list__
                     (CFunc (list 'self) (none)
                            (CReturn (CBuiltinPrim 'tuple-list
                                         (list
                                           (CId 'self (LocalId)))))
                            (some 'tuple)))

                  (def '__tuple__
                     (CFunc (list 'self) (none)
                            (CReturn (CBuiltinPrim 'tuple-tuple
                                         (list
                                           (CId 'self (LocalId)))))
                            (some 'tuple)))

                  (def '__str__
                       (CFunc (list 'self) (none)
                              (CReturn (CBuiltinPrim 'tuple-str
                                                     (list (CId 'self (LocalId)))))
                              (some 'tuple)))
                  (def '__getitem__
                    (CFunc (list 'self 'idx) (none)
                           (CReturn (CBuiltinPrim 'tuple-getitem
                                                  (list
                                                   (CId 'self (LocalId))
                                                   (CId 'idx (LocalId)))))
                           (some 'tuple)))

                  ;;; tuple comparisons - taken verbatim from list comparisons ;;;
                  (def '__cmp__
                    (CFunc (list 'self 'other) (none)
                           (CLet 'listcmp (CNone)
                             (seq-ops (list
                               (def 'listcmp
                                    (CFunc (list 'self 'other 'idx) (none)
                                           (seq-ops (list
                                             (def 'li1
                                                  (CApp (CGetField (CId 'self (LocalId))
                                                                   '__getitem__)
                                                        (list (CId 'idx (LocalId)))
                                                        (none)))
                                             (def 'li2
                                                  (CApp (CGetField (CId 'other (LocalId))
                                                                   '__getitem__)
                                                        (list (CId 'idx (LocalId)))
                                                        (none)))
                                             (CIf (CPrim2 'Is (CId 'li1 (LocalId)) (CNone))
                                                  (CIf (CPrim2 'Is (CId 'li2 (LocalId)) (CNone))
                                                       (CReturn (make-builtin-num 0))
                                                       (CReturn (make-builtin-num -1)))
                                                  (CIf (CPrim2 'Is (CId 'li2 (LocalId)) (CNone))
                                                       (CReturn (make-builtin-num 1))
                                           (seq-ops (list
                                             (def 'cmpval
                                                  (CApp (CGetField (CId 'li1 (LocalId))
                                                                   '__cmp__)
                                                        (list (CId 'li2 (LocalId)))
                                                        (none)))
                                             (CIf (CApp (CGetField (CId 'cmpval (LocalId))
                                                                   '__eq__)
                                                        (list (make-builtin-num 0))
                                                        (none))
                                                  (seq-ops (list 
                                                    (def 'nidx
                                                         (CApp (CGetField (CId 'idx (LocalId))
                                                                          '__add__)
                                                               (list (make-builtin-num 1))
                                                               (none)))
                                                    (CReturn 
                                                      (CApp (CId 'listcmp (LocalId))
                                                          (list (CId 'self (LocalId))
                                                                (CId 'other (LocalId))
                                                                (CId 'nidx (LocalId)))
                                                          (none)))))
                                                  (CReturn (CId 'cmpval (LocalId))))))))))
                                           (none)))
                               (CReturn 
                                 (CApp (CId 'listcmp (LocalId))
                                     (list (CId 'self (LocalId))
                                           (CId 'other (LocalId))
                                           (make-builtin-num 0))
                                     (none))))))
                           (some 'tuple)))
                  (def '__eq__
                    (CFunc (list 'self 'other) (none)
                           (seq-ops (list
                                      (def '_cmpresult
                                           (CApp (CGetField (CId 'self (LocalId)) '__cmp__)
                                                 (list (CId 'other (LocalId)))
                                                 (none)))
                                      (CReturn (CApp (CGetField (CId '_cmpresult (LocalId))
                                                                '__eq__)
                                                     (list (make-builtin-num 0))
                                                     (none)))))
                           (some 'tuple)))

                  ;;; end tuple comparisons ;;;
))))

(define (make-builtin-tuple [l : (listof CVal)]) : CVal
  (VObject 'tuple
           (some (MetaTuple l))
           (make-hash empty)))

;; convert a tuple to a list
(define (tuple-list (args : (listof CVal)) 
                    [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'tuple
              (some 
                (make-builtin-list (MetaTuple-v mval1)))))

; returns the tuple itself
; that way, tuple.__tuple__() "is" itself
(define (tuple-tuple (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'tuple
               (some (first args))))

(define (tuple+ (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'tuple 'tuple
               (some (VObject 'tuple
                              (some (MetaTuple
                                     (append (MetaTuple-v mval1)
                                             (MetaTuple-v mval2))))
                              (hash empty)))))

(define (tuple* (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'tuple 'num
               (letrec ([tuple-list (MetaTuple-v mval1)]
                        [repetitions (MetaNum-n mval2)]
                        [repeat (lambda ([lst : (listof CVal)] [reps : number]) : (listof CVal)
                                  (cond
                                    [(= reps 0) (list)]
                                    [(= reps 1) lst]
                                    [(> reps 1) (append lst (repeat lst (sub1 reps)))]))])
               (some (VObject 'tuple
                              (some (MetaTuple
                                     (repeat tuple-list repetitions)))
                              (hash empty))))))

(define (tuple-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'tuple
               (some (VObject 'num
                              (some (MetaNum (length (MetaTuple-v mval1))))
                              (hash empty)))))

(define (tuple-in [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
 (letrec ([self-list (MetaTuple-v (some-v (VObject-mval (first args))))]
          [test (second args)]
          [contains (lambda ([lst : (listof CVal)] [val : CVal]) : CVal
                    (cond
                     [(empty? lst) false-val]
                     [(cons? lst)
                       (if (equal? val (first lst))
                         true-val
                         (contains (rest lst) val))]))])
   (some (contains self-list test))))

(define (tuple-getitem (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  ; TODO: slicing
  (check-types args env sto 'tuple 'num
               (some
                 (try (list-ref (MetaTuple-v mval1) (MetaNum-n mval2))
                      (lambda () vnone)))))

(define (tuple-str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'tuple
               (some (VObject 'str
                        (some (MetaStr
                                (pretty-metaval mval1)))
                        (make-hash empty)))))

