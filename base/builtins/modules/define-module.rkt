#lang plai-typed

(require "../../python-core-syntax.rkt"
         "../../util.rkt"
         (typed-in racket/pretty (pretty-print : ('a -> 'b)))
         (typed-in racket/base (append : ((listof 'a) (listof 'a) -> (listof 'a)))))
 
; define-module: symbol * symbol * (listof CExpr CExpr) ... => CExpr
;                returns a opteration sequence to construct
;                a module object
;         usage: refers to sys.rkt
(define-syntax (define-module stx)
  (syntax-case stx ()
    [(_ internal-name bound-to (attr val) next ...)
     #'(define internal-name
         (append
          (list
           (CAssign (CId bound-to (GlobalId)) (CObject '$module (none)))
           (CAssign (CGetField (CId bound-to (GlobalId)) attr) val))
          (define-module bound-to next ...)))]
    [(_ bound-to (attr val) next ...)
     #'(append (list
                (CAssign 
                 (CGetField (CId bound-to (GlobalId)) attr)
                 val))
               (define-module bound-to next ...))]
    [(_ bound-to)
     #'(list)]))
        
;;; make-dict: (listof CExpr CExpr) *  ... => (CDict (hashof CExpr CExpr))
;       usage: (make-dict (list (CStr "sys") (CId '$sys (GlobalId)))
;                         (list (CStr "imp") (CId '$imp (GlobalId)))))
(define-syntax make-dict
  (syntax-rules ()
    [(make-dict (list k v))
     (CDict (hash (list (values k v))))]
    [(make-dict (list k v) next ...)
     (CDict (hash (make-pairs (list k v) next ...)))]))

;;; make-dict helper macro:
(define-syntax make-pairs
  (syntax-rules ()
    [(make-pairs (list k v))
     (values k v)]
    [(make-pairs (list k v) next ...)
     (list (values k v)
           (make-pairs next ...))]))


