#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt")

;; cascade-lets will build up the nested lets, and use body as the
;; eventual body, preserving order of evaluation of the expressions
(define (cascade-lets (ids : (listof symbol))
                      (sts : (listof ScopeType))
                      (exprs : (listof CExp))
                      (body : CExp)) : CExp
  (cond [(empty? ids) body]
        [(cons? ids)
         (CLet (first ids) (first sts) (first exprs) (cascade-lets (rest ids) (rest sts) (rest exprs) body))]))


(define (get-vars [expr : PyExpr]) : (listof (ScopeType * symbol))
  (type-case PyExpr expr
    [PyNonlocal (ids) (map (lambda (id) (values (NonLocal) id)) ids)]
    [PyGlobal (ids) (map (lambda (id) (values (Global) id)) ids)]
    [PyGlobalEnv () (list)]
    [PySeq (es) (foldl (lambda (a b) (append b a))
                       (list)
                       (map (lambda (e) (get-vars e)) es))]
    [PyNum (n) (list)]
    [PyApp (f args keywordArguments star) (append (get-vars star) 
                                                      (append (get-vars f)
                                                              (foldl (lambda (a b) (append b a))
                                                                     (list)
                                                                     (map (lambda (e) (get-vars e)) 
                                                                          (append args
                                                                                  (map (lambda (kwarg) (type-case keywargHelperType kwarg
                                                                                                         [keywarghelpertype (id value) value]))
                                                                                       keywordArguments))))))]
    [PyTuple (elts) 
             (foldl (lambda (a b) (append b a))
                    (list)
                    (map (lambda (e) (get-vars e)) elts))]
    [PyCollectionSet (elts)
                     (foldl (lambda (a b) (append b a))
                            (list)
                            (map (lambda (e) (get-vars e)) elts))]
    [PyReturn (value) (get-vars value)]
    [PyBreak () (list)]
    [PyContinue () (list)]
    [PyId (id) (list)]; (values (NotReallyLocal) id))]
    [PyStr (s) (list)]
    [PyBinOp (op left right)
             (append
              (get-vars left)
              (get-vars right))]
    [PyAttribute (attr value)
                 (get-vars value)]
    [PySubscript (value attr)
                 (append (get-vars value)
                         (get-vars attr))]
    [PySlice (lower upper step)
             (append (get-vars lower)
                     (append (get-vars upper)
                             (get-vars step)))]
    [PyDel (targets) (list)]
    
    ;; loops
    [PyWhile (test body orelse) (append (get-vars test)
                                        (get-vars body))] ;; really?
    [PyFor (target iter body)
           (append (type-case PyExpr target
                               [PyId (id) (list (values (Local) id))]
                               [else (get-vars target)])
                             (get-vars body))]
    [PyListComp (elt generators) (foldl (lambda (a b) (append b a))
                                        (list)
                                        (map (lambda (e) (get-vars e)) generators))]
    [PyComprehension (target iter) (type-case PyExpr target
                                     [PyId (id) (list (values (Local) id))]
                                     [else (get-vars target)])]
    
    [PyIf (test then orelse)
          (append
           (get-vars test)
           (append
            (foldl (lambda (a b) (append b a))
                   (list)
                   (map (lambda (e) (get-vars e)) then))
            (foldl (lambda (a b) (append b a))
                   (list)
                   (map (lambda (e) (get-vars e)) orelse))))]
    [PyBoolop (op exprs)
              (foldl (lambda (a b) (append b a))
                                   (list)
                                   (map (lambda (e) (get-vars e)) exprs))]
    [PyCompare (left ops comparators)
               (append
                (get-vars left)
                (foldl (lambda (a b) (append b a))
                                   (list)
                                   (map (lambda (e) (get-vars e)) comparators)))]
    [PyPass () (list)]
    [PyNone () (list)]
    [PyHolder (expr) (list)]
    [PyLambda (args body) (get-vars args)]

    [PyDef (name args body classmethod)
           (append (list (values (Local) name))
                   (get-vars args))]

    
    
    [PyClassDef (name bases body) (list (values (Local) name))]
    
    
    [PyArguments (args defaults vararg)          ;-------------------------------------------check if declarations in defaults should be caught
                 (list)]
    
    [PyRaise (exc) (get-vars exc)]
    [Py-NotExist () (list)]
    [PyUnaryOp (op arg) (get-vars arg)]
    
    [PySet (lhs value) ;;PySet case may need to change, because it never actually appears since it only exists from use in PyAssign
           (append
               (get-vars value)
               (type-case PyExpr lhs
                 [PyId (id) (list (values (Local) id))]
                 [else (list)]))]
    [PyAssign (targets value)
              (append
               (get-vars value)
               (foldl (lambda (a b) (append b a))
                      (list)
                      (map (lambda (e) (type-case PyExpr e
                                         [PyId (id) (list (values (Local) id))]
                                         [PyTuple (tuple-elts) (foldl (lambda (a b) (append b a))
                                                                (list)
                                                                (map (lambda (e) (type-case PyExpr e
                                                                                   [PyId (id) (list (values (Local) id))]
                                                                                   [else (list)])) 
                                                                     tuple-elts))]
                                         [else (list)])) 
                           targets)))]
    
    [PyAugAssign (target op value)
                 (append
                  (get-vars value)
                  (get-vars target))]
    
    [PyModule (exprs)
              (get-vars exprs)]
    
    [PyList (elts) 
            (foldl (lambda (a b) (append b a))
                   (list)
                   (map (lambda (e) (get-vars e)) elts))]
    
    [PyDict (keys vals) 
            (append (foldl (lambda (a b) (append b a))
                           (list)
                           (map (lambda (e) (get-vars e)) keys))
                    (foldl (lambda (a b) (append b a))
                           (list)
                           (map (lambda (e) (get-vars e)) vals)))]
    [PyTryExcept (body handlers orelse)
                 (append (get-vars body)
                         (append (get-vars orelse)
                                 (foldl (lambda (a b) (append b a))
                                        (list)
                                        (map (lambda (e) (get-vars (PyExcHandler-body e))) handlers))))] ;; Check... TODO
    [PyTryFinally (body finalbody)
                  (append (get-vars body)
                          (get-vars finalbody))] ;; ???
   ; [PyExceptHandler (name type body)
  ;                   (append (list (values (Local) name))
  ;                           (append (get-vars type)
  ;                                   (foldl (lambda (a b) (append b a))
  ;                                          (list)
  ;                                          (map (lambda (e) (get-vars e)) body))))]
    ;[else (error 'get-vars "Case not implemented")]
    ))


;; separate get-vars for inside of a class...
;; ADDED for freevar-in-method
;(define (get-vars-class [expr : PyExpr]) : (listof (ScopeType * symbol))
;  (type-case PyExpr expr
;    [PySeq (es) (foldl (lambda (a b) (append b a))
;                       (list)
;                       (map (lambda (e) (get-vars-class e)) es))]
;    [PyDef (name args body)
;           (append (list (values (Instance) name))
;                   (get-vars args))]
;    [else (get-vars expr)])) ;; I hope this works...


(define (desugar expr)
  (type-case PyExpr expr
    ;   #|
    [PySeq (es) (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first es)) (rest es))]
    [PyNum (n) (CNum n)]
    [PyApp (f args keywordArguments star) (CApp (desugar f) 
                                                    (map desugar args)
                                                    (map (lambda (keyArg) (type-case keywargHelperType keyArg
                                                                            [keywarghelpertype (arg value) (values arg (desugar value))]))
                                                         keywordArguments)
                                                   ; (CHash (hash (list)) (cType "list" (list)))
                                                    (desugar star)
                                                    )]
    [PyId (x) (CId x)]
    ;;Under this, Non-TA code
    [PyStr (s) (CStr s)]
    [PyIf (test then orelse)
          (CIf (desugar test) 
               (if (> (length then) 1) (desugar (PySeq then)) (desugar (first then)))
               (if (> (length orelse) 0) 
                   (if (> (length orelse) 1)
                       (desugar (PySeq orelse)) 
                       (desugar (first orelse)))
                   (CPass)))]
    [PyGlobal (ids) (CPass)]
    [PyNonlocal (ids) (CPass)]
    [PyBoolop (op exprs)
              (case op
                ['or (foldl (lambda (expr result) (CPrim2 'or result (desugar expr))) (desugar (first exprs)) (rest exprs))]
                ['and (foldl (lambda (expr result) (CPrim2 'and result (desugar expr))) (desugar (first exprs)) (rest exprs))])]
    [PyUnaryOp (op arg)
               (CApp (CId op) 
                     (list (desugar arg)) 
                     (list) 
                     (CHash (hash (list)) (cType "list" (CId 'list)))
                     )] ;; TODO this needs to desugar to a function application
    [PyBinOp (op left right)
             (CApp (CId op) 
                   (list (desugar left) (desugar right)) 
                   (list) 
                   (CHash (hash (list)) (cType "list" (CId 'list)))
                   )]
    [PyCompare (left ops comparators)
               (if (equal? 0 (length comparators))
                   (CTrue)
                   (CLet 'left-comp (Local) (desugar left)
                     (CLet 'right-comp (Local) (desugar (first comparators))
                       (CIf (CApp (CId (first ops))
                                  (list (CId 'left-comp) (CId 'right-comp))
                                  (list)
                                  (CHash (hash (list)) (cType "list" (CId 'list)))
                                  )
                            (desugar (PyCompare (PyId 'right-comp)
                                                (rest ops)
                                                (rest comparators)))
                            (CFalse)))))]
    
    [PyPass () (CPass)]
    [PyNone () (CNone)]
    [PyLambda (args body) (CFunc (PyArguments-args args) (desugar body) (list) (map desugar (PyArguments-defaults args)) false (PyArguments-vararg args))]
    [PyArguments (args defaults vararg) (CPass)] ;just used within PyLambda and PyDef
    
    [PyRaise (exc) (CError (desugar exc))]
    [PyAssign (targets value) 
              (if (PyTuple? (first targets))
                  (desugar-assign-tuple targets value)
                  (CLet 'assign-value (Local) (desugar value)
                        (desugar (PySeq (map (lambda (e) (PySet e (PyId 'assign-value))) targets)))))]
    [PyAugAssign (target op value)
                 (CLet 'aug-value (Local) (desugar value)
                       (CLet 'orig-value (Local) (desugar target)
                             (CSet (desugar target) (CApp (CId op) 
                                                          (list (CId 'orig-value) (CId 'aug-value))
                                                          (list)
                                                          (CHash (hash (list)) (cType "list" (CId 'list)))
                                                          ))))] 
                              ;; may or may not work - side effects?
    
    
    [PySet (lhs value) 
           (CSet (desugar lhs) (desugar value))]
           
    [PyGlobalEnv () (CGlobalEnv)]
    [PyModule (exprs) 
              (let ([global-vars (get-vars exprs)]) ;gets all of the assignments in the global scope
                (begin (if (hasGlobalScopeErrors global-vars) ;checks the existence of 'nonlocal' or 'global' declarations in the global scope
                           (error 'PyModule "Global or Nonlocal declaration in the global scope.")
                           (void))
                       (cascade-lets (get-ids global-vars) ;puts the variables in the environment as Globals
                                     (make-item-list (Global) (length global-vars) (list)) 
                                     (make-item-list (CUnbound) (length global-vars) (list)) 
                                     (desugar (PySeq (append 
                                                      (list (PyGlobalEnv)) ;the first thing interpreter does is creating the 
                                                                           ;separate global environment
                                                      (list exprs)))))))] ;executes the program
    
    [PyDef (name args body classmethod) 
           (begin (CSeq
                   (CSet (CId name) (CFunc (list) (CError (CStr "dummy function was called!")) (list) (list) false 'no-vararg))
                   (CLet 'some-func 
                         (Local) 
                         (CFunc (PyArguments-args args) 
                                (desugar body) 
                                (get-vars body) 
                                (map desugar (PyArguments-defaults args))
                                classmethod
                                (PyArguments-vararg args))
                         (CSet (CId name) (CId 'some-func)))))]
    
   ; c.f
    [PyClassDef (name bases body) 
                (begin (CSeq
                        (CSet (CId name) (CHash (hash (list)) (cType "dummy" (CUnbound))))
                        (CLet 'some-class 
                              (Local) 
                              (CHash (hash (list (values (CStr "__name__") (CStr (symbol->string name)))
                                               ;  (values (CStr "super") (CFunc (list)
                                               ;                                (if (empty? bases)
                                               ;                                    (CId '_Object)
                                               ;                                    (desugar (first bases)))
                                               ;                                (list)
                                               ;                                (list)
                                               ;                                'no-vararg))
                                                 )) 
                                     (cType "class" (if (empty? bases)
                                                       (CId '_Object)
                                                       (desugar (first bases)))))
                              ;;body of the CLet:
                              (CSeq
                               (CSet (CId name) (CId 'some-class))
                               (CCreateClass name (desugar body) (get-vars body))))))] ;; ADDED for freevar-in-method
                             ;  (CCreateClass (desugar-class-innards name body  )))))]
    
    
    [PyList (elts) (CHash (hash-set (desugar-hash (pynum-range (length elts)) elts) (CStr "__size__") (CNum (length elts))) (cType "list" (CId 'list)))]
    [PyDict (keys vals) (CHash (hash-set (hash-set (desugar-hash keys vals) 
                                                   (CStr "__size__") 
                                                   (CNum (length keys)))
                                         (CStr "__keys__") 
                                         (desugar (PyCollectionSet keys))) 
                               (cType "_dict" (CId '_dict)))]
    [PyTuple (elts) (CHash (hash-set (desugar-hash (pynum-range (length elts)) elts) (CStr "__size__") (CNum (length elts))) (cType "tuple" (CId 'tuple)))]
    [PyCollectionSet (elts) (CHash (desugar-hash elts elts) (cType "set" (CId 'set)))]
    
    [PyAttribute (attr value) (CAttribute attr (desugar value))]
    [PySubscript (value attr) 
                 (type-case PyExpr attr
                   [PySlice (lower upper step) (CApp (CId 'make-slice)
                                                     (list (CApp (CId 'list)
                                                                 (list (desugar value))
                                                                 (list)
                                                                 (Empty-list)) 
                                                           (desugar lower) 
                                                           (desugar upper) 
                                                           (desugar step))
                                                     (list)
                                                     (Empty-list))]
            ;                (CLet 'build-string
            ;                      (Local)
            ;                      (CStr "")
            ;                      (CLet 'e-elem
            ;                            (Local)
            ;                            (CNum 0)
            ;                            (desugar (PySeq 
            ;                                      (list (PyFor (PyId 'e-elem)
            ;                                                   (PyApp (PyId 'range) 
            ;                                                          (list lower 
            ;                                                                (PyIf (PyCompare upper 
            ;                                                                                 (list 'python-lte) 
            ;                                                                                 (list (PyNum 0)))
            ;                                                                      (list (PyApp (PyId 'len)
            ;                                                                                   (list value)
            ;                                                                                   (list)
            ;                                                                                   (PyList (list))))
            ;                                                                      (list upper))
            ;                                                                step) 
            ;                                                          (list) 
            ;                                                          (PyList (list)))
            ;                                                   (PyAugAssign (PyId 'build-string) 
            ;                                                                'python-add 
           ;                                                                 (PySubscript value (PyId 'e-elem))))
            ;                                            (PyReturn (PyId 'build-string)))))))]
                   [else (CSubscript (desugar value) (desugar attr))])]
    [PySlice (lower upper step) (error 'desugar "lone, wandering PySlice...")]
    [PyDel (target)
           (CDel (map desugar target))]
    ;; loops
    [PyWhile (test body orelse) (CWhile (desugar test) (desugar body) (desugar orelse) (list))]
    
    [PyFor (target iter body)
           (CLet '_it
                 (Local)
                 (CApp (CId 'iter) 
                       (list (desugar iter))
                       (list)
                       (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list))))
                 (CTryExcept (CWhile (CTrue)
                                     (CSeq (CSet (desugar target)
                                                 (CApp (CId 'next)
                                                       (list (CId '_it))
                                                       (list)
                                                       (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list)))))
                                           (desugar body))
                                     (CPass)
                                     (list))
                             (list (CExcHandler 'no-name
                                                (CId 'StopIteration)
                                                (CPass))) ;[CExcHandler (name : symbol) (type : CExp) (body : CExp)]
                             (CPass)))]

    [PyListComp (elt generators)
                ;create an empty list (LST),
                ;create a loop with the fors
                ;the inner-most body of the loop will be (LST.append(elt))
                (CLet '_lst
                      (Local)
                      (CApp (CId 'list)
                            (list)
                            (list)
                            (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list))))
                      (CSeq (desugar (loop-listcomp elt generators))
                            (CId '_lst)))]
   
    ;; exceptions
    [PyTryExcept (body handlers orelse) (CTryExcept (desugar body) (map desugar-handler handlers) (desugar orelse))]
    [PyTryFinally (body finalbody) (CTryFinally (desugar body) (desugar finalbody))]
  ;  [PyExceptHandler (name type body) (CPass)]
    
    ;; return
    [PyReturn (value) (CReturn (desugar value))]
    [PyBreak () (CBreak)]
    [PyContinue () (CContinue)]
    [PyHolder (exp) exp]
                
    [else (error 'desugar (string-append "Haven't desugared a case yet:\n"
                                       (to-string expr)))]))

(define (desugar-assign-tuple [targets : (listof PyExpr)]
                              [value : PyExpr]) : CExp
  (CLet 'assign-value 
        (Local) 
        (desugar value)
        (desugar (PySeq
                  (list (PySet (first (PyTuple-elts (first targets))) (PySubscript (PyId 'assign-value) (PyNum 0))) 
                        (PySet (second (PyTuple-elts (first targets))) (PySubscript (PyId 'assign-value) (PyNum 1))))))))
  #|
  (type-case PyExpr value
    [PyTuple (elts) (desugar (PySeq (desugar-tuple-pysets (PyTuple-elts (first targets)) elts)))]
    [else (CError (CApp (CId 'TypeError)
                        (list)
                        (list)
                        (Empty-list)))]))
|#

(define (desugar-tuple-pysets [targets : (listof PyExpr)]
                              [vals : (listof PyExpr)]) : (listof PyExpr)
  (cond
    [(or (empty? targets) (empty? vals)) (list)]
    [else (cons (PySet (first targets) (first vals)) (desugar-tuple-pysets (rest targets) (rest vals)))]))


(define (loop-listcomp [elt : PyExpr]
                       [generators : (listof PyExpr)]) : PyExpr
  (cond
    [(empty? generators) (PyApp (PyAttribute 'append (PyId '_lst)) ;the _lst identifier comes from the declared list in the [PyListComp] case in the desugarer
                                (list elt)
                                (list)
                                (PyHolder (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list)))))]
    [else (type-case PyExpr (first generators)
            [PyComprehension (target iter) 
                             (PyFor target iter (loop-listcomp elt (rest generators)))]
            [else (error 'desugar-listcomp "every generator should be a PyComprehension")])]))
     



#|
;; desugar class innards
(define (desugar-class-innards (name : symbol) (body : (listof PyExpr)) ) : CExp
  (CCreateClass name (foldl (lambda (e1 e2) (CSeq e2 (desugar e1))) (desugar (first body)) (rest body))))
;  (cond 
;    [(empty? body) ...]
;    [(= (length body) 1) ()]
;    [else (CSeq () (desugar-class-innards name (rest body)))]))
|#

;; desugar class body
;(define (desugar-class-body (body : (listof PyExpr))) : (hashof CExp CExp)
;  (


;; desugars handlers
(define (desugar-handler [handler : PyExceptHandler]) : CExceptionHandler
  (CExcHandler (PyExcHandler-name handler) (desugar (PyExcHandler-type handler)) (desugar (PyExcHandler-body handler))))

;; desugars a dictionary
(define (desugar-hash [keys : (listof PyExpr)]
                      [vals : (listof PyExpr)]) : (hashof CExp CExp)
  (cond
    [(and (empty? keys) (empty? vals)) (hash (list))]
    [(and (cons? keys) (cons? vals)) (hash-set (desugar-hash (rest keys) (rest vals)) (desugar (first keys)) (desugar (first vals)))]
    [else (error 'desugar-hash "key and value lists do not match")]))

;; helper functions to create ranges of numbers
;###########################
(define (pynum-range [n : number]) : (listof PyExpr)
  (map (lambda (x) (PyNum x)) (range n)))

(define (range [n : number]) : (listof number)
  (reverse (range-backwards n)))

(define (range-backwards [n : number]) : (listof number)
  (cond
    [(<= n 0) empty]
    [else (cons (- n 1) (range-backwards (- n 1)))]))
;###########################

(define (make-item-list [item : 'a]
                        [size : number]
                        [newList : (listof 'a)]) : (listof 'a)
  (cond 
    [(>= (length newList) size) newList]
    [else (make-item-list item size (append (list item) newList))]))

(define (get-ids [vars-list : (listof (ScopeType * symbol))]) : (listof symbol)
  (foldl (lambda (a b) (append b a))
                       (list)
                       (map (lambda (e) (local ([define-values (st id) e])
                                                      (list id)))
                              vars-list)))

(define (get-sts [vars-list : (listof (ScopeType * symbol))]) : (listof ScopeType)
  (foldl (lambda (a b) (append b a))
                       (list)
                       (map (lambda (e) (local ([define-values (st id) e])
                                                      (list st)))
                              vars-list)))


;; hasGlobalScopeErrors checks the declarations in the global environment.
;; If we have something that is not a 'Local', we return true ('we have an error').
(define (hasGlobalScopeErrors [vars : (listof (ScopeType * symbol))]) : boolean
  (not (foldl (lambda (list-el result) (and list-el result))
              true
              (map (lambda (e) (local ([define-values (st id) e])
                                 (Local? st) ))
                   vars))))


;(test (desugar (PyBoolop 'or (list (PyNum 0) (PyNum 1))))
;      (CBoolop 'or (CNum 0) (CNum 1)))