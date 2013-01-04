#lang plai-typed

(require "../python-core-syntax.rkt"
         "../util.rkt"
         "num.rkt")
(require (typed-in racket/base (string=? : (string string -> boolean)))
         (typed-in racket/base (string>? : (string string -> boolean)))
         (typed-in racket/base (string<? : (string string -> boolean)))
         (typed-in racket/base (string-length : (string -> number)))
         (typed-in racket/string (string-replace : (string string string -> string)))
         (typed-in racket/base (make-string : (number char -> string)))
         (typed-in racket/base (string->list : (string -> (listof char))))
         (typed-in racket/base (char->integer : (char -> number)))
         (typed-in racket/base (abs : (number -> number)))
         (typed-in racket/base (integer->char : (number -> char)))
         (typed-in racket/base (andmap : ( ('a -> boolean) (listof 'a) -> boolean))))

(define str-class : CExpr
  (CClass
   'str 
   'object
  (seq-ops (list 
                  (def '__init__
                       (CFunc (list 'self 'other) (none) 
                              (CAssign
                                (CId 'self (LocalId))
                                (CApp (CGetField (CId 'other (LocalId)) '__str__)
                                             (list (CId 'other (LocalId)))
                                             (none)))
                              true))
                  (def '__add__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'str+
                                                  (list
                                                   (CId 'self (LocalId))
                                                   (CId 'other (LocalId)))))
                           true))
                  (def '__mult__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'str*
                                         (list
                                          (CId 'self (LocalId))
                                          (CId 'other (LocalId)))))
                           true))
                  (def '__iter__
                       (CFunc (list 'self) (none)
                           (CReturn (CApp (CGetField (CId 'SeqIter (LocalId)) '__init__)
                                          (list (CObject 'SeqIter (none)) 
                                                (CId 'self (LocalId)))
                                          (none)))
                           true))
                  (def '__str__
                       (CFunc (list 'self) (none)
                              (CReturn (CId 'self (LocalId)))
                              true))
                  (def '__eq__
                    (CFunc (list 'self 'other) (none)
                           (CReturn (CBuiltinPrim 'str=
                                         (list
                                          (CId 'self (LocalId))
                                          (CId 'other (LocalId)))))
                           true))
                  (def '__cmp__
                     (CFunc (list 'self 'other) (none)
                            (CReturn (CBuiltinPrim 'strcmp
                                         (list
                                           (CId 'self (LocalId))
                                           (CId 'other (LocalId)))))
                            true))
                  (def '__in__
                     (CFunc (list 'self 'test) (none)
                            (CReturn (CBuiltinPrim 'strin
                                         (list
                                           (CId 'self (LocalId))
                                           (CId 'test (LocalId)))))
                            true))
                  (def '__min__
                     (CFunc (list 'self) (none)
                            (CReturn (CBuiltinPrim 'strmin
                                         (list
                                           (CId 'self (LocalId)))))
                            true))
                  (def '__max__
                     (CFunc (list 'self) (none)
                            (CReturn (CBuiltinPrim 'strmax
                                         (list
                                           (CId 'self (LocalId)))))
                            true))
                  (def '__len__
                     (CFunc (list 'self) (none)
                            (CReturn (CBuiltinPrim 'strlen
                                         (list
                                           (CId 'self (LocalId)))))
                            true))

                  (def '__list__
                     (CFunc (list 'self) (none)
                            (CReturn (CBuiltinPrim 'strlist
                                         (list
                                           (CId 'self (LocalId)))))
                            true))
                  (def '__tuple__
                     (CFunc (list 'self) (none)
                            (CReturn (CBuiltinPrim 'str-tuple
                                         (list
                                           (CId 'self (LocalId)))))
                            true))
                  (def '__getitem__
                     (CFunc (list 'self 'idx) (none)
                            (CReturn (CBuiltinPrim 'str-getitem
                                         (list
                                           (CId 'self (LocalId))
                                           (CId 'idx (LocalId)))))
                            true))
                 (def '__slice__
                    (CFunc (list 'self 'lower 'upper 'step) (none)
                        (CReturn (CBuiltinPrim 'strslice
                                    (list 
                                      (CId 'self (LocalId))
                                      (CId 'lower (LocalId))
                                      (CId 'upper (LocalId))
                                      (CId 'step (LocalId)))))
                        true)))))) 

(define (make-builtin-str [s : string]) : CExpr
  (CObject
   'str
   (some (MetaStr s))))

(define (make-str-value [s : string]) : CVal
  (VObject
    'str
    (some (MetaStr s))
    (hash empty)))

(define (string->charlist [str : string]) : (listof CVal)
  (map (lambda (s)
               (VObject 'str
                        (some (MetaStr (make-string 1 s)))
                        (make-hash empty)))
       (string->list str)))

(define (strlist [args : (listof CVal)] [env : Env] [sto : Store])
  : (optionof CVal)
  (check-types args env sto 'str
               (some (VObject 'list
                              (some (MetaList (string->charlist (MetaStr-s mval1))))
                              (make-hash empty)))))

(define (str-tuple [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str
               (some (VObject 'tuple
                              (some (MetaTuple (string->charlist (MetaStr-s mval1))))
                              (make-hash empty)))))

(define (str+ (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str 'str
               (some (VObject 'str 
                              (some (MetaStr
                                     (string-append (MetaStr-s mval1)
                                                    (MetaStr-s mval2))))
                              (hash empty)))))

(define (str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str
               (some (VObject 'str
                              (some (MetaStr (MetaStr-s mval1)))
                              (hash empty)))))

(define (str* (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  ;(let ([str*l 
  (check-types args env sto 'str 'num
                      (some (VObject 'str
                                     (some (MetaStr 
                                      (str*-rec (MetaStr-s mval1)
                                                (MetaNum-n mval2))))
                                     (hash empty)))))
    #|(if (none? str*l)
      (check-types args env sto 'num 'str
             (some (VObject 'str
                            (some (MetaStr 
                             (str*-rec (MetaStr-s mval2)
                                       (MetaNum-n mval1))))
                            (hash empty))))
      str*l)))|#

(define (str*-rec [str : string] [num : number]) : string
  (cond
    [(<= num 0) ""]
    [else (string-append str (str*-rec str (sub1 num)))]))

(define (strcmp [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str 'str
     (some (VObject 'num
                    (some (MetaNum
                      (let ([str1 (MetaStr-s mval1)]
                            [str2 (MetaStr-s mval2)])
                        (cond
                          [(string<? str1 str2) -1]
                          [(string>? str1 str2) 1]
                          [(string=? str1 str2) 0]))))
                    (hash empty)))))
(define (streq [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str 'str 
               (let ([str1 (MetaStr-s mval1)] 
                     [str2 (MetaStr-s mval2)]) 
                 (if (string=? str1 str2) 
                   (some true-val) 
                   (some false-val)))))

(define (strin [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str 'str
     (let ([self (MetaStr-s mval1)]
           [test (MetaStr-s mval2)])
       (some (if (or (< (string-length (string-replace self test ""))
                     (string-length self))
                     (string=? test ""))
                 true-val
                 false-val)))))

(define (strlen [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str
     (some (VObject 'num
                    (some (MetaNum
                            (string-length (MetaStr-s mval1))))
                    (hash empty)))))

(define (strbool [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str
     (some (if (string=? (MetaStr-s mval1) "")
               false-val
               true-val))))

(define (strmin [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str
     (some (VObject 'str
                    (some (MetaStr
                            (make-string 1
                              (integer->char
                                (foldl (lambda (c res)
                                         (min res c))
                                         ;; the maximum char integer is currently #x10FFFF
                                         ;; should find a better way to do this
                                         #x110000
                                         (map char->integer
                                          (string->list (MetaStr-s mval1))))))))
                    (hash empty)))))

(define (strmax [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str
     (some (VObject 'str
                    (some (MetaStr
                            (make-string 1
                              (integer->char
                                (foldl (lambda (c res)
                                         (max res c))
                                       -1
                                       (map char->integer
                                          (string->list (MetaStr-s mval1))))))))
                    (hash empty)))))

(define (str-getitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  ; handle slicing here?  
  (check-types args env sto 'str 'num
     (some (VObject 'str
                    (some (MetaStr
                            (make-string 1
                                         (string-ref 
                                           (MetaStr-s mval1)
                                           (MetaNum-n mval2)))))
                    (hash empty)))))
;; compute a slice of a string, should have 4 args, the string and 3 nums for
;; the start, end and step size of the slice
(define (strslice [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (if (and (andmap (lambda(a) (VObject? a)) args)
          (and (andmap (lambda(o) (some? (VObject-mval o))) args)
               (and (MetaStr? (some-v (VObject-mval (first args))))
                    (and (andmap (lambda(o) 
                                   (or (MetaNum? (some-v (VObject-mval o)))
                                       (MetaNone? (some-v (VObject-mval o)))))
                                 (rest args))))))
    (local [(define str (MetaStr-s (some-v (VObject-mval (first args)))))
            (define step (if (MetaNum? (some-v (VObject-mval (fourth args))))
                           (MetaNum-n (some-v (VObject-mval (fourth args))))
                           1))

            (define start-i (if (MetaNum? (some-v (VObject-mval (second args))))
                              (MetaNum-n (some-v (VObject-mval (second args))))
                              (if (> step 0)
                                0
                                (+ 1 (string-length str)))))

            (define end-i (if (MetaNum? (some-v (VObject-mval (third args))))
                              (MetaNum-n (some-v (VObject-mval (third args))))
                              (if (> step 0)
                                (+ 1 (string-length str))
                                -1)))

            (define indices (filter (lambda(n) (and (< n (string-length str))
                                                    (>= n 0)))
                              (build-list (abs (- end-i start-i))
                                            (lambda(n) (+ (* n step) start-i)))))

            (define char-list (if (> step 0)
                                (string->list str)
                                (string->list str)))]

           (some (VObject 'str 
                    (some 
                      (MetaStr (list->string (map 
                                             (lambda(i) (list-ref char-list i))
                                             indices))))
                    (make-hash empty))))
    (none)))
