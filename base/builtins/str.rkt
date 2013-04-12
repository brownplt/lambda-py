#lang plai-typed/untyped

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
         (typed-in racket/base (andmap : ( ('a -> boolean) (listof 'a) -> boolean)))
         (typed-in racket/base (string->number : (string number -> number)))
         (typed-in racket/list (take : ((listof 'a) number -> (listof 'a))))
         (typed-in racket/list (fifth : ((listof 'a) -> 'a)))
         (typed-in racket/base (integer? : (number -> boolean))))

(define (make-str-value [s : string] [str-cls : CVal]) : CVal
  (VObjectClass
    'str
    (some (MetaStr s))
    (hash empty)
    (some str-cls)))

(define (str+ (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaStr? MetaStr?
               (some (VObjectClass 'str 
                              (some (MetaStr
                                      (string-append (MetaStr-s mval1)
                                                     (MetaStr-s mval2))))
                              (hash empty)
                              (some (third args))))))

;; str: creates a normal str but, if %str is not bound yet, it creates an
;; internal str useful for bootstrapping.
(define (str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaStr?
               (some (VObjectClass 'str
                              (some (MetaStr (MetaStr-s mval1)))
                              (hash empty)
                              (type-case (optionof Address) (lookup '%str env)
                                [none () (none)]
                                [some (w) (let ([str_cls (fetch-once w sto)])
                                            (if (VUndefined? str_cls)
                                                (none)
                                                (some str_cls)))])))))

(define (str* (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaStr? MetaNum?
               (some (VObjectClass 'str
                              (some (MetaStr 
                                      (str*-rec (MetaStr-s mval1)
                                                (MetaNum-n mval2))))
                              (hash empty)
                              (some (third args))))))

(define (str*-rec [str : string] [num : number]) : string
  (cond
    [(<= num 0) ""]
    [else (string-append str (str*-rec str (sub1 num)))]))

(define (strcmp [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaStr? MetaStr?
               (some (VObjectClass 'int
                              (some (MetaNum
                                      (let ([str1 (MetaStr-s mval1)]
                                            [str2 (MetaStr-s mval2)])
                                        (cond
                                          [(string<? str1 str2) -1]
                                          [(string>? str1 str2) 1]
                                          [(string=? str1 str2) 0]))))
                              (hash empty)
                              (some (third args))))))

(define (streq [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaStr? MetaStr? 
               (let ([str1 (MetaStr-s mval1)] 
                     [str2 (MetaStr-s mval2)]) 
                 (if (string=? str1 str2) 
                     (some true-val) 
                     (some false-val)))))

(define (strin [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaStr? MetaStr?
               (let ([self (MetaStr-s mval1)]
                     [test (MetaStr-s mval2)])
                 (some (if (or (< (string-length (string-replace self test ""))
                                  (string-length self))
                               (string=? test ""))
                           true-val
                           false-val)))))

(define (strlen [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaStr?
     (some (VObjectClass 'int
                    (some (MetaNum
                            (string-length (MetaStr-s mval1))))
                    (hash empty)
                    (some (second args))))))

(define (strbool [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaStr?
     (some (if (string=? (MetaStr-s mval1) "")
               false-val
               true-val))))

(define (strint [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaStr?
     (let ([n  (string->number (MetaStr-s mval1) 10)])
       (if (integer? n)
           (some (make-builtin-numv n))
           (none)))))

(define (strmin [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaStr?
     (some (VObjectClass 'str
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
                    (hash empty)
                    (some (second args))))))

(define (strmax [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaStr?
     (some (VObjectClass 'str
                    (some (MetaStr
                            (make-string 1
                              (integer->char
                                (foldl (lambda (c res)
                                         (max res c))
                                       -1
                                       (map char->integer
                                          (string->list (MetaStr-s mval1))))))))
                    (hash empty)
                    (some (second args))))))

(define (str-getitem [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  ; handle slicing here?  
  (check-types-pred args env sto MetaStr? MetaNum?
     (some (VObjectClass 'str
                    (some (MetaStr
                            (make-string 1
                                         (string-ref 
                                           (MetaStr-s mval1)
                                           (MetaNum-n mval2)))))
                    (hash empty)
                    (some (third args))))))

(define (hash-a-str str)
  (let* ([chars (string->list str)]
         [ints (map (lambda (c) (* (char->integer c) 37)) chars)])
   (foldl + 0 ints)))

(define (str-hash [args : (listof CVal)] [env : Env] [sto : Store])
  : (optionof CVal)
  (check-types-pred args env sto MetaStr?
    (some (VObjectClass 'num
            (some (MetaNum (hash-a-str (MetaStr-s mval1))))
            (hash empty)
            (some (second args))))))

;; compute a slice of a string, should have 4 args, the string and 3 nums for
;; the start, end and step size of the slice
(define (strslice [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (if (and (andmap (lambda(a) (VObjectClass? a)) (take args (- (length args) 1)))
          (and (andmap (lambda(o) (some? (VObjectClass-mval o))) (take args (- (length args) 1)))
               (and (MetaStr? (some-v (VObjectClass-mval (first args))))
                    (and (andmap (lambda(o) 
                                   (or (MetaNum? (some-v (VObjectClass-mval o)))
                                       (MetaNone? (some-v (VObjectClass-mval o)))))
                                 (take (rest args) (- (length (rest args)) 1)))))))
    (local [(define str (MetaStr-s (some-v (VObjectClass-mval (first args)))))
            (define step (if (MetaNum? (some-v (VObjectClass-mval (fourth args))))
                           (MetaNum-n (some-v (VObjectClass-mval (fourth args))))
                           1))

            (define start-i (if (MetaNum? (some-v (VObjectClass-mval (second args))))
                              (MetaNum-n (some-v (VObjectClass-mval (second args))))
                              (if (> step 0)
                                0
                                (+ 1 (string-length str)))))

            (define end-i (if (MetaNum? (some-v (VObjectClass-mval (third args))))
                              (MetaNum-n (some-v (VObjectClass-mval (third args))))
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

           (some (VObjectClass 'str 
                    (some 
                      (MetaStr (list->string (map 
                                             (lambda(i) (list-ref char-list i))
                                             indices))))
                    (hash empty)
                    (some (fifth args)))))
    (none)))
