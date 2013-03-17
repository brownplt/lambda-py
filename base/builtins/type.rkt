#lang plai-typed/untyped

(require "../python-core-syntax.rkt")
(require "../util.rkt")
(require [typed-in racket/list (remove-duplicates : ((listof 'a) -> (listof 'a)))])

;; builtin-class: used to construct builtin classes in the core language
(define (builtin-class [name : symbol] [bases : (listof symbol)] [body : CExpr]) : CExpr
  (make-class name
              ;; builtin classes are bound to ids in the global scope
              (CTuple (CNone) ;; we may not have a tuple class object yet
                      (map (lambda (id) (CId id (GlobalId))) bases))
              body))

;; make-class: used to build class objects
;; - create an empty class object using type-new
;; - check uniqueness of bases using type-uniqbases and set __bases__ field
;; - compute linearization of bases using type-buildmro and set __mro__ field
(define (make-class [name : symbol] [bases : CExpr] [body : CExpr]) : CExpr
  (CLet
   'bases (LocalId) bases
   (CLet
    'new-class (LocalId)
    (CBuiltinPrim 'type-new
                  (list (CObject (CNone)
                                 (some (MetaStr (symbol->string name))))))
    (CIf
     (CBuiltinPrim 'type-uniqbases (list (CId 'bases (LocalId))))
     (CSeq
      (CAssign (CGetField (CId 'new-class (LocalId)) '__bases__)
               (CId 'bases (LocalId)))
      (CTryExceptElse
       (CAssign (CGetField (CId 'new-class (LocalId)) '__mro__)
                (CBuiltinPrim 'type-buildmro (list
                                              (CTuple (CNone) ;; we may not have tuple yet
                                                      (list (CId 'new-class (LocalId))))
                                              (CId 'bases (LocalId)))))
       '_ (CRaise (some (make-exception 'TypeError "duplicate base")))
       (CId 'new-class (LocalId))))
     (CRaise (some (make-exception
                    'TypeError
                    "cannot create a consistent method resolution order")))))))

;; type-new: creates a new class object
;; first argument: the name as a string
(define (type-new [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaStr?
                    (let ([name (string->symbol (MetaStr-s mval1))])
                      (some (VObjectClass 'type
                                          (some (MetaClass name))
                                          (hash empty)
                                          (none))))))

;; type-uniqbases: check for uniqueness of bases
;; first argument: the tuple of bases
(define (type-uniqbases [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaTuple?
                    (let ([bases (MetaTuple-v mval1)])
                      (some (if (= (length (remove-duplicates bases)) (length bases))
                                true-val
                                false-val)))))

;; type-buildmro: merge the __mro__ of the bases using the C3 algorithm
;; first argument: a tuple containing the class
;; second argument: the tuple of bases
(define (type-buildmro (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaTuple? MetaTuple?
                    (let* ([cls-list (MetaTuple-v mval1)]
                           [bases (MetaTuple-v mval2)]
                           ;; mro tail is the c3-merge of the mro of the bases plus the list of bases
                           [maybe-mro (c3-merge (append (map (lambda (base) (get-mro base (none) sto))
                                                             bases)
                                                        (list bases)) empty)])
                      (if (some? maybe-mro)
                          (some (VObjectClass
                                 'tuple
                                 ;; final result is prepended with the class itself
                                 (some (MetaTuple (append cls-list (some-v maybe-mro))))
                                 (hash empty)
                                 (none))) ;; tuple class object may not be available yet
                          (none)))))

;; c3-merge: implements the c3 algorithm to merge mro lists
;; looks for a candidate (using c3-select)) and removes it from the mro lists
;; until all the mro lists are empty (success) or no candidate can be found (fail).
(define (c3-merge [xss : (listof (listof 'a))] 
                  [acc : (listof 'a)]) : (optionof (listof 'a))
  (let ([xss-ne (filter cons? xss)])
    (cond
      [(empty? xss-ne) (some acc)]
      [else (type-case (optionof 'b) (c3-select xss-ne 0)
              [none () (none)]
              [some (el) (c3-merge
                          (map (lambda (xs) 
                                 (filter (lambda (x) (not (eq? x el))) xs)) 
                               xss-ne)
                          (append acc (list el)))])])))

;; c3-select: looks sequentially for a head which doesn't appear in the tails
;; if none is found there is no c3 linearization possible
(define (c3-select [xss : (listof (listof 'a))] [n : number]) : (optionof 'a)
  (cond
    [(>= n (length xss)) (none)]
    [else (let ([el (first (list-ref xss n))])
            (if (any (map (lambda (xs) (member el (rest xs))) xss))
                (c3-select xss (add1 n))
                (some el)))]))
