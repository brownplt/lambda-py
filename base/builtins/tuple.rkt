#lang plai-typed/untyped

(require "../python-core-syntax.rkt")
(require "../util.rkt"
         "num.rkt"
         "list.rkt")
(require [typed-in racket (format : (string 'a -> string))])

(define (make-builtin-tuple [l : (listof CVal)]) : CVal
  (VObject 'tuple
           (some (MetaTuple l))
           (hash empty)))

(define (tuple+ (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaTuple? MetaTuple?
               (some (VObjectClass
                              'tuple
                              (some (MetaTuple
                                     (append (MetaTuple-v mval1)
                                             (MetaTuple-v mval2))))
                              (hash empty)
                              (some (third args))))))

(define (tuple* (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaTuple? MetaNum?
               (letrec ([tuple-list (MetaTuple-v mval1)]
                        [repetitions (MetaNum-n mval2)]
                        [repeat (lambda ([lst : (listof CVal)] [reps : number]) : (listof CVal)
                                  (cond
                                    [(= reps 0) (list)]
                                    [(= reps 1) lst]
                                    [(> reps 1) (append lst (repeat lst (sub1 reps)))]))])
               (some (VObjectClass 'tuple
                              (some (MetaTuple
                                     (repeat tuple-list repetitions)))
                              (hash empty)
                              (some (third args)))))))

(define (tuple-len (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (type-case CVal (first args)
    [VObjectClass (_ mv dic cls)
     (type-case MetaVal (some-v mv)
      [MetaTuple (vals)
       (some
        (VObjectClass 'int
                      (some (MetaNum (length vals)))
                      (hash empty)
                      (some (second args))))]
      [else (error 'tuple-len (format "Got non meta-tuple: ~a" mv))])]
    [else (error 'tuple-len (format "Got non-object: ~a" (first args)))]))

(define (tuple-getitem (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  ; TODO: slicing
  (check-types-pred args env sto MetaTuple? MetaNum?
                 (try (some (list-ref (MetaTuple-v mval1) (MetaNum-n mval2)))
                      (lambda () (none)))))

(define (tuple-set (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaTuple?
               (let ([values (MetaTuple-v mval1)])
                    (some (VObjectClass 'set
                                   (some (MetaSet (make-set values)))
                                   (hash empty)
                                   (some (second args)))))))

(define (tuple-str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaTuple?
               (some (VObjectClass 'str
                        (some (MetaStr
                                (pretty-metaval mval1 sto)))
                        (hash empty)
                        (some (second args))))))

