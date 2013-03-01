#lang plai-typed

;; method - the bound method object, includes classmethod, staticmethod and super.

(require "../python-core-syntax.rkt" 
         "../util.rkt")

;; method type
;; In method objects __func__ and __self__ are defined as instance attributes
;; __call__ attribute is the method object itself
(define method-class
  (seq-ops (list
             (CAssign (CId 'method (GlobalId))
                      (CClass 
                        'method
                        (list 'object)
                        (CNone)))
             (def 'method '__init__ 
                  (CFunc (list 'self 'func 'obj) (none)
                         (CSeq
                           (CAssign (CGetField (CId 'self (LocalId)) '__func__)
                                    (CId 'func (LocalId)))
                           (CAssign (CGetField (CId 'self (LocalId)) '__self__)
                                    (CId 'obj (LocalId))))
                         (some 'method))))))

;; mk-method: creates a method object with func and self attributes,
;; it is special-cased to avoid circulariy in object creation.
;; The optional address parameter is used to support self aliasing
(define (mk-method [w_func : Address] [self : CVal] [opt_w_self : (optionof Address)]
                   [sto : Store]) : (CVal * Store)
  (local ([define w_self (if (some? opt_w_self) (some-v opt_w_self) (new-loc))]
          [define self_sto (if (none? opt_w_self) (hash-set sto w_self self) sto)])
    (values
     (VObjectClass 'method
              (none)
              (hash 
               (list [values '__func__ w_func]
                     [values '__self__ w_self]))
              (none))
     self_sto)))

;; classmethod type
;; In classmethod objects __func__ is defined as instance attribute
;; classmethod objects are converted to method objects 
;; with class as __self__ on attribute retrieval
(define classmethod-class
  (seq-ops (list
            (CAssign (CId 'classmethod (GlobalId))
                     (CClass
                       'classmethod
                       (list 'object)
                       (CNone)))
            (def 'classmethod '__init__
                 (CFunc (list 'self 'func) (none)
                        (CAssign (CGetField (CId 'self (LocalId)) '__func__)
                                 (CId 'func (LocalId)))
                        (some 'classmethod))))))

;; staticmethod type
;; In staicmethod objects __func__ is defined as instance attribute
;; staticmethod objects are converted to functions on attribute retrieval
(define staticmethod-class
  (seq-ops (list
             (CAssign (CId 'staticmethod (GlobalId))
                      (CClass
                        'staticmethod
                        (list 'object)
                        (CNone)))
             (def 'staticmethod '__init__
                  (CFunc (list 'self 'func) (none)
                         (CAssign (CGetField (CId 'self (LocalId)) '__func__)
                                  (CId 'func (LocalId)))
                         (some 'staticmethod))))))

;; super type
;; super proxy object, implemented only in the zero arguments version
;; whick depends on local variables current-class and current-self
;; to be set in methods bodies to class and first argument respectively.
(define super-class
  (seq-ops (list
             (CAssign (CId 'super (GlobalId))
                      (CClass
                        'super
                        (list 'object)
                        (CNone)))
             ;; for the 1/2 argument version __init__ needs some changes
             (def 'super '__init__
                  (CFunc (list 'self) (none)
                         (CSeq
                           (CAssign (CGetField (CId 'self (LocalId)) '__thisclass__)
                                    (CBuiltinPrim '$thisclass (list)))
                           (CAssign (CGetField (CId 'self (LocalId)) '__self__)
                                    (CBuiltinPrim '$self (list))))
                         ;; self and thisclass must be from the calling environment,
                         ;; so mark this method as a function.
                         (none))))))
