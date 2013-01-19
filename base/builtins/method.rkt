#lang plai-typed

;; method - the bound method object, includes classmethod, staticmethod and super.

(require "../python-core-syntax.rkt" 
         "../util.rkt")

;; method type
;; In method objects __func__ and __self__ are defined as instance attributes
;; __call__ attribute is the method object itself
(define method-class
  (CClass 
   'method
   (list 'object)
   (seq-ops (list
             [def '__init__ 
                    (CFunc (list 'self 'func 'obj) (none)
                      (CSeq
                       (CAssign (CGetField (CId 'self (LocalId)) '__func__)
                                (CId 'func (LocalId)))
                       (CAssign (CGetField (CId 'self (LocalId)) '__self__)
                                (CId 'obj (LocalId))))
                      (some 'method))]))))

;; classmethod type
;; In classmethod objects __func__ is defined as instance attribute
;; classmethod objects are converted to method objects 
;; with class as __self__ on attribute retrieval
(define classmethod-class
  (CClass
   'classmethod
   (list 'object)
   (seq-ops (list
             [def '__init__
               (CFunc (list 'self 'func) (none)
                      (CAssign (CGetField (CId 'self (LocalId)) '__func__)
                               (CId 'func (LocalId)))
                      (some 'classmethod))]))))

;; staticmethod type
;; In staicmethod objects __func__ is defined as instance attribute
;; staticmethod objects are converted to functions on attribute retrieval
(define staticmethod-class
  (CClass
   'staticmethod
   (list 'object)
   (seq-ops (list
             [def '__init__
               (CFunc (list 'self 'func) (none)
                      (CAssign (CGetField (CId 'self (LocalId)) '__func__)
                               (CId 'func (LocalId)))
                      (some 'staticmethod))]))))

;; super type
;; super proxy object, implemented only in the zero arguments version
;; whick depends on local variables current-class and current-self
;; to be set in methods bodies to class and first argument respectively.
(define super-class
  (CClass
   'super
   (list 'object)
   (seq-ops (list
             ;; for the 1/2 argument version the __new__ needs (easy) changes
             [def '__init__
               (CFunc (list 'self) (none)
                      (CSeq
                       (CAssign (CGetField (CId 'self (LocalId)) '__thisclass__)
                                (CBuiltinPrim '$thisclass (list)))
                       (CAssign (CGetField (CId 'self (LocalId)) '__self__)
                                (CBuiltinPrim '$self (list))))
                      (some 'super))]))))
