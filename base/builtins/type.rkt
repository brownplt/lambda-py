#lang plai-typed/untyped

(require "../python-core-syntax.rkt")
(require "../util.rkt")
(require [typed-in racket/list (remove-duplicates : ((listof 'a) -> (listof 'a)))])

;; builtin-class: used to construct builtin classes in the core language
(define (builtin-class [name : symbol] [bases : (listof symbol)] [body : CExpr]) : CExpr
  (make-class (GlobalId) name
              ;; builtin classes are bound to ids in the global scope
              (CTuple (CNone) ;; we may not have a tuple class object yet
                      (map (lambda (id) (CId id (GlobalId))) bases))
              body))

;; make-class: used to build class objects and assign them to name in scope
;; - create an empty class object using type-new and assign it to name in scope
;; - check uniqueness of bases using type-uniqbases and set __bases__ field
;; - compute linearization of bases using type-buildmro and set __mro__ field
;; - execute the class body
(define (make-class [scope : IdType] [name : symbol] [bases : CExpr] [body : CExpr]) : CExpr
  (CSeq
   (CAssign (CId name scope)
            (CBuiltinPrim 'type-new
                          (list (CObject (CNone)
                                         (some (MetaStr (symbol->string name)))))))
   (CTryExceptElse
    (set-field (CId name scope) '__bases__ (CBuiltinPrim 'type-uniqbases (list bases)))
    '_ (CRaise (some (make-exception 'TypeError "duplicate base")))
    ;; no exception, bases are not duplicated, compute __mro__
    (CTryExceptElse
     (set-field (CId name scope) '__mro__
                (CBuiltinPrim 'type-buildmro
                              (list
                               (CTuple (CNone) ;; we may not have tuple yet
                                       (list (CId name scope)))
                               (CBuiltinPrim 'obj-getattr (list (CId name scope)
                                                                (make-pre-str "__bases__"))))))
     '_ (CRaise (some (make-exception 'TypeError
                                      "cannot create a consistent method resolution order")))
     ;; no exception, the class object is created and assigned to name, execute the class body
     (CSeq body (CId name scope))))))

;; type-new: creates a new class object
;; first argument: the name as a string
;; second argument: the optional metaclass
(define (type-new [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaStr?
                    (let ([name (string->symbol (MetaStr-s mval1))])
                      (if (and (= (length args) 2) (is-obj-ptr? (second args) sto))
                          (some (VObjectClass (get-class-name (fetch-ptr (second args) sto))
                                              (some (MetaClass name))
                                              (hash empty)
                                              (some (second args))))
                          (some (VObjectClass '%type
                                              (some (MetaClass name))
                                              (hash empty)
                                              (none)))))))

(define (get-class-name [cls : CVal]) : symbol
  (if (and (some? (VObjectClass-mval cls)) (MetaClass? (some-v (VObjectClass-mval cls))))
      (MetaClass-c (some-v (VObjectClass-mval cls)))
      (error 'get-class-name "not a class object")))

(define (type-metaclass [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaClass?
                    (type-case (optionof CVal) (VObjectClass-class (first args))
                      [none () (some vnone)]
                      [some (mc) (some mc)])))

;; type-uniqbases: check for uniqueness of bases
;; first argument: the tuple of bases
(define (type-uniqbases [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaTuple?
                    (let ([bases (MetaTuple-v mval1)])
                      (if (= (length (remove-duplicates bases)) (length bases))
                          (some (VObjectClass
                                 'tuple
                                 (some (MetaTuple bases))
                                 (hash empty)
                                 (none))) ;; tuple class object may not be available yet
                          (none)))))

;; type-buildmro: merge the __mro__ of the bases using the C3 algorithm
;; first argument: a tuple containing the class
;; second argument: the tuple of bases
(define (type-buildmro (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaTuple? MetaTuple?
                    (let* ([cls-list (MetaTuple-v mval1)]
                           [bases (MetaTuple-v mval2)]
                           ;; mro tail is the c3-merge of the mro of the bases plus the list of bases
                           [maybe-mro (c3-merge (append (map (lambda (base) (get-mro base sto))
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
