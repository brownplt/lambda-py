#lang plai-typed/untyped

(require "../python-core-syntax.rkt")
(require "../util.rkt"
         "none.rkt"
         "num.rkt"
         "list.rkt")

(define (make-builtin-tuple [l : (listof CVal)]) : CVal
  (VObject 'tuple
           (some (MetaTuple l))
           (hash empty)))

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
                        (hash empty)))))

