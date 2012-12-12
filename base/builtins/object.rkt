#lang plai-typed

;; object - the base-class of everything
(require "../python-core-syntax.rkt" 
         "../util.rkt"
         "num.rkt"
         "str.rkt"
         (typed-in racket/base (string-length : (string -> number))))

(define object-class
  (CClass 
    'object
    'no-super
    (seq-ops (list
               (def '__init__ 
                    (CFunc (list 'self) (none)
                           (CId 'self (LocalId))
                           true))
               (def '__eq__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CPrim2 'Is
                                            (CId 'self (LocalId))
                                            (CId 'other (LocalId))))
                           true))

               (def '__str__ 
                    (CFunc (list 'self)  (none)
                           (CReturn (CBuiltinPrim 'obj-str (list (CId
                                                                   'self (LocalId)))))
                           true))

               (def '__cmp__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CIf (CPrim2 'Is
                                            (CId 'self (LocalId))
                                            (CId 'other (LocalId)))
                                         (make-builtin-num 0)
                                         (make-builtin-num -1)))
                           true))

              (def '__gt__
                    (CFunc (list 'self 'other) (none)
                           (CSeq (CAssign (CId '_cmpresult (LocalId))
                                    (CApp (CGetField (CId 'self (LocalId)) '__cmp__)
                                          (list (CId 'self (LocalId)) (CId 'other (LocalId)))
                                          (none)))
                                 (CReturn (CApp (CGetField (CId '_cmpresult (LocalId)) '__gt__)
                                            (list (CId '_cmpresult (LocalId))
                                                  (make-builtin-num 0))
                                            (none))))
                           true))
               (def '__lt__
                    (CFunc (list 'self 'other) (none)
                           (CSeq (CAssign (CId '_cmpresult (LocalId))
                                    (CApp (CGetField (CId 'self (LocalId)) '__cmp__)
                                          (list (CId 'self (LocalId)) (CId 'other (LocalId)))
                                    (none)))
                                 (CReturn (CApp (CGetField (CId '_cmpresult (LocalId)) '__lt__)
                                            (list (CId '_cmpresult (LocalId))
                                                  (make-builtin-num 0))
                                            (none))))
                           true))
               (def '__lte__
                    (CFunc (list 'self 'other) (none)
                           (CSeq (CAssign (CId '_cmpresult (LocalId))
                                    (CApp (CGetField (CId 'self (LocalId)) '__cmp__)
                                          (list (CId 'self (LocalId)) (CId 'other (LocalId)))
                                          (none)))
                                 (CReturn (CApp (CGetField (CId '_cmpresult (LocalId))
                                                           '__lte__)
                                            (list (CId '_cmpresult (LocalId))
                                                  (make-builtin-num 0))
                                            (none))))
                           true))
              (def '__iter__
                   (CFunc (list 'self) (none)
                       (CReturn (CApp (CGetField (CId 'SeqIter (LocalId)) '__init__)
                                      (list (CObject 'SeqIter (none)) 
                                            (CId 'self (LocalId)))
                                      (none)))
                       true))
               (def '__gte__
                    (CFunc (list 'self 'other) (none)
                           (CSeq (CAssign (CId '_cmpresult (LocalId))
                                    (CApp (CGetField (CId 'self (LocalId)) '__cmp__)
                                          (list (CId 'self (LocalId)) (CId 'other (LocalId)))
                                          (none)))
                                 (CReturn (CApp (CGetField (CId '_cmpresult (LocalId))
                                                           '__gte__)
                                            (list (CId '_cmpresult (LocalId))
                                                  (make-builtin-num 0))
                                            (none))))
                           true))))))


;; produces true-val if the object is truthy and false-val if it is not
(define (truthy-object? [o : CVal]) : boolean
  (if (some? (VObject-mval o))
    (let ([mval (some-v (VObject-mval o))])
      (type-case MetaVal mval
                 [MetaNum (n) (if (= n 0) false true)]
                 [MetaStr (s) (if (= (string-length s) 0) false true)]
                 [MetaList (v) (if (= (length v) 0) false true)]
                 [MetaTuple (v) (if (= (length v) 0) false true)]
                 [MetaDict (contents) (if (= (length (hash-keys contents)) 0) false true)]
                 [MetaNone () false]
                 [else true]))
   true))

(define (obj-str (args : (listof CVal))) : (optionof CVal)
  (local [(define o (first args))]
         (type-case CVal o
            [VObject (ante mval d)
                     (some (VObject 'str
                        (if (and (some? mval) (MetaClass? (some-v mval)))
                            (some (MetaStr (string-append "<class "
                                           (string-append (symbol->string
                                                            (MetaClass-c (some-v mval)))
                                                          ">"))))
                            (some (MetaStr
                                    (string-append "<instance of " 
                                                   (string-append 
                                                     (if (symbol=? ante 'none)
                                                       "Object"
                                                       (symbol->string ante)) ">")))))
                        (make-hash empty)))]
            [else (error 'obj-str "Non object")])))
