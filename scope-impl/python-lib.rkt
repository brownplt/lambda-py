#lang plai-typed

(require "python-desugar.rkt")
(require "python-syntax.rkt")
(require "python-core-syntax.rkt")

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#

(define-type-alias Lib (CExp -> CExp))

(define (One-list [ele : CExp]) : CExp
  (CHash (hash (list (values (CStr "__size__") (CNum 1)) (values (CNum 0) ele))) (cType "list" (CId 'list))))

(define (Make-tuple-pair [ele1 : CExp] [ele2 : CExp]) : CExp
  (CHash (hash (list (values (CStr "__size__") (CNum 2)) (values (CNum 0) ele1) (values (CNum 1) ele2))) (cType "tuple" (CId 'tuple))))

(define (Create-test-sub [ele : CExp]) : CExp
  (CPrim2 'or 
          (CPrim2 'or 
                  (CPrim2 'eq (CPrim1 'tagof ele) (CStr "list"))
                  (CPrim2 'eq (CPrim1 'tagof ele) (CStr "tuple")))
          (CPrim2 'eq (CPrim1 'tagof ele) (CStr "set"))))

(define (Create-for-loop [target : symbol] [iter : CExp] [body : CExp]) : CExp
  (CLet target
        (Local)
        (CUnbound)
        (CLet '_it
              (Local)
              (CApp (CId 'iter) 
                    (list iter)
                    (list)
                    (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list))))
              (CTryExcept (CWhile (CTrue)
                                  (CSeq (CSet (CId target)
                                              (CApp (CId 'next)
                                                    (list (CId '_it))
                                                    (list)
                                                    (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list)))))
                                        body)
                                  (CPass)
                                  (list))
                          (list (CExcHandler 'no-name
                                             (CId 'StopIteration)
                                             (CPass))) ;[CExcHandler (name : symbol) (type : CExp) (body : CExp)]
                          (CPass)))))

(define print-lambda
  (CFunc (list 'to-print)
         (CPrim1 'print (CId 'to-print)) (list) (list) false 'no-vararg))

(define assert-equal-lambda
  (CFunc (list 'e-1 'e-2)
         (CIf (CApp (CId 'python-eq) 
                    (list (CId 'e-1) (CId 'e-2)) 
                    (list) 
                    (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list)))
                    )
              (CPass) 
              (CError (CApp (CId 'Exception)
                            (list (CPrim2 'string+ 
                                          (CStr "Assert failed: values are not Equal: ")
                                          (CPrim2 'string+ 
                                                  (CApp (CId 'str) 
                                                        (list (CId 'e-1))
                                                        (list)
                                                        (Empty-list))
                                                  (CApp (CId 'str) 
                                                        (list (CId 'e-2))
                                                        (list)
                                                        (Empty-list)))))
                            (list)
                            (Empty-list)))
              ) 
         (list)
         (list)
         false
         'no-vararg))

(define assert-notEqual-lambda
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'eq (CId 'e-1) (CId 'e-2))  
              (CError (CStr "Assert failed: values are Equal"))
              (CPass)
              )
         (list)
         (list)
         false
         'no-vararg))

(define assert-true-lambda
  (CFunc (list 'check-true)
         (CIf (CId 'check-true) (CPass) (CError (CStr "Assert failed: value is False")))
         (list)
         (list)
         false
         'no-vararg))

(define assert-false-lambda
  (CFunc (list 'check-false)
         (CIf (CId 'check-false) (CError (CStr "Assert failed: value is True")) (CPass) )
         (list)
         (list)
         false
         'no-vararg))

(define assert-is-lambda
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'is (CId 'e-1) (CId 'e-2)) 
              (CPass) 
              (CError (CStr "Assert failed: first argument is not second argument"))
              )
         (list)
         (list)
         false
         'no-vararg))

(define assert-isNot-lambda
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'is (CId 'e-1) (CId 'e-2)) 
              (CError (CStr "Assert failed: first argument is second argument"))
              (CPass) 
              )
         (list)
         (list)
         false
         'no-vararg))

(define assert-in-lambda
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'in (CId 'e-1) (CId 'e-2)) 
              (CPass) 
              (CError (CStr "Assert failed: element not found"))
              )
         (list)
         (list)
         false
         'no-vararg))

(define assert-notIn-lambda
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'is (CId 'e-1) (CId 'e-2)) 
              (CError (CStr "Assert failed: element found"))
              (CPass)
              )
         (list)
         (list)
         false
         'no-vararg))


(define assert-raises-lambda
  (CFunc (list 'e-1 'e-2)
         (CTryExcept (CApp (CId 'e-2) (list) (list) (CId 'vargs)) 
                     (list (CExcHandler 'e (CId 'e-1) (CTrue)))
                     (CFalse))
         (list)
         (list)
         false
         'vargs))

;; TODO can this be re-written with instanceof?

;(define assert-raises-lambda
;  (CFunc (list 'e-1 'e-2)
; (CTryExcept (CApp (CId 'e-2) (CPrim1 'python-star (CId 'args)) (list)) (CExcHandler 'e () (CTrue)) (CFalse))
;; Or however it ends up working...
; (list)
; (list)
; 'args))



;; math
(define python-add
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'and
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "string")))
              (CPrim2 'string+ (CId 'e-1) (CId 'e-2))
              (CIf (CPrim2 'or
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float")))
                   (CIf (CPrim2 'or
                                (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                                (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float")))
                        (CPrim2 'num+ (CId 'e-1) (CId 'e-2))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "bool"))
                             (CPrim2 'num+ (CId 'e-1) (CPrim1 'to-int (CId 'e-2)))
                             (CError (CStr "+: Cannot do math on this type!"))))
                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))
                        (CIf (CPrim2 'or
                                     (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                                     (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float")))
                             (CPrim2 'num+ (CPrim1 'to-int (CId 'e-1)) (CId 'e-2))
                             (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "bool"))
                                  (CPrim2 'num+ (CPrim1 'to-int (CId 'e-1)) (CPrim1 'to-int (CId 'e-2)))
                                  (CError (CStr "+: Cannot do math on this type!"))))
                        (CIf (CPrim2 'and 
                                     (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "list"))
                                     (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "list")))
                             (CPrim2 'list+ (CId 'e-1) (CId 'e-2))
                             (CIf (CPrim2 'and 
                                          (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "tuple"))
                                          (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "tuple")))
                                  (CPrim2 'tuple+ (CId 'e-1) (CId 'e-2))
                                  (CError (CStr "+: Cannot do math on this type... Sorry!")))))))
         (list)
         (list)
         false
         'no-vararg))


;; handles addition
;(define python-add
;  (CFunc (list 'e-1 'e-2)
;         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CPrim1 'tagof (CId 'e-2)))
;              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
;                   (CPrim2 'num+ (CId 'e-1) (CId 'e-2))
;                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float"))
;                        (CPrim2 'num+ (CId 'e-1) (CId 'e-2))
;                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))
;                             (CPrim2 'string+ (CId 'e-1) (CId 'e-2))
;                             (CError (CStr "+: Not supported for this type.")))))
;              (CError (CStr "+: Types do not match.")))))

(define python-sub
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'or
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float")))
              (CIf (CPrim2 'or
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float")))
                   (CPrim2 'num- (CId 'e-1) (CId 'e-2))
                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "bool"))
                        (CPrim2 'num- (CId 'e-1) (CPrim1 'to-int (CId 'e-2)))
                        (CError (CStr "-: Cannot do math on this type!"))))
              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))
                   (CIf (CPrim2 'or
                                (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                                (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float")))
                        (CPrim2 'num- (CPrim1 'to-int (CId 'e-1)) (CId 'e-2))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "bool"))
                             (CPrim2 'num- (CPrim1 'to-int (CId 'e-1)) (CPrim1 'to-int (CId 'e-2)))
                             (CError (CStr "-: Cannot do math on this type!"))))
                   (CIf (CPrim2 'and (Create-test-sub (CId 'e-1)) (Create-test-sub (CId 'e-2))) 
                        (CApp (CId 'subtract-sets-helper)
                              (list (CId 'e-1) (CId 'e-2))
                              (list)
                              (Empty-list))
                        (CError (CStr "-: Cannot do math on this type... Sorry!")))))
         (list)
         (list)
         false
         'no-vararg))

;; helper function for set subtraction
(define subtract-sets-helper
  (CFunc (list 'e-1 'e-2)
         (CLet 'sub-iter
               (Local)
               (CApp (CId 'iter)
                     (list (CId 'e-1))
                     (list)
                     (Empty-list))
               (CLet 'build-list ;; Needs to be finished... 
                     (Local)
                     (Empty-list)
                     (CTryExcept (CWhile (CTrue)
                                         (CLet 'e-curr
                                               (Local)
                                               (CApp (CId 'next)
                                                     (list (CId 'sub-iter))
                                                     (list)
                                                     (Empty-list))
                                               (CIf (CApp (CId 'python-notIn)
                                                          (list (CId 'e-curr) (CId 'e-2))
                                                          (list)
                                                          (Empty-list))
                                                    (CApp (CAttribute 'append (CId 'build-list))
                                                          (list (CId 'e-curr))
                                                          (list)
                                                          (Empty-list))
                                                    (CPass)))
                                         (CPass)
                                         (list))
                                 (list (CExcHandler 'no-name (CId 'StopIteration) (CReturn (CApp (CId 'set)
                                                                                                 (list (CId 'build-list))
                                                                                                 (list)
                                                                                                 (Empty-list)))))
                                 (CPass))))
         (list)
         (list)
         false
         'no-vararg))

;; bit and is here, since it also works on lists...
(define python-bitand
  (CFunc (list 'e-1 'e-2)
         (CLet 'sub-iter
               (Local)
               (CApp (CId 'iter)
                     (list (CPrim2 'list+ 
                                   (CApp (CId 'list)
                                         (list (CId 'e-1)) 
                                         (list)
                                         (Empty-list))
                                   (CApp (CId 'list)
                                         (list (CId 'e-2))
                                         (list)
                                         (Empty-list))))
                     (list)
                     (Empty-list))
               (CLet 'build-list ;; Needs to be finished... 
                     (Local)
                     (Empty-list)
                     (CTryExcept (CWhile (CTrue)
                                         (CLet 'e-curr
                                               (Local)
                                               (CApp (CId 'next)
                                                     (list (CId 'sub-iter))
                                                     (list)
                                                     (Empty-list))
                                               (CIf (CPrim2 'and
                                                            (CApp (CId 'python-in)
                                                                  (list (CId 'e-curr) (CId 'e-2))
                                                                  (list)
                                                                  (Empty-list))
                                                            (CApp (CId 'python-in)
                                                                  (list (CId 'e-curr) (CId 'e-1))
                                                                  (list)
                                                                  (Empty-list)))
                                                    (CApp (CAttribute 'append (CId 'build-list))
                                                          (list (CId 'e-curr))
                                                          (list)
                                                          (Empty-list))
                                                    (CPass)))
                                         (CPass)
                                         (list))
                                 (list (CExcHandler 'no-name (CId 'StopIteration) (CReturn (CApp (CId 'set)
                                                                                                 (list (CId 'build-list))
                                                                                                 (list)
                                                                                                 (Empty-list)))))
                                 (CPass))))
         (list)
         (list)
         false
         'no-vararg))



; (CFunc (list 'e-1 'e-2)
;        (CError (Make-throw 'Exception "Bitwise and not implemented yet!"))
;        (list)
;       (list)
;        'no-vararg))

(define python-bitor ;; This can be simplified; we don't really need to iterator. Just concat and setify. 
  (CFunc (list 'e-1 'e-2)
         (CLet 'sub-iter
               (Local)
               (CApp (CId 'iter)
                     (list (CPrim2 'list+ 
                                   (CApp (CId 'list)
                                         (list (CId 'e-1)) 
                                         (list)
                                         (Empty-list))
                                   (CApp (CId 'list)
                                         (list (CId 'e-2))
                                         (list)
                                         (Empty-list))))
                     (list)
                     (Empty-list))
               (CLet 'build-list ;; Needs to be finished... 
                     (Local)
                     (Empty-list)
                     (CTryExcept (CWhile (CTrue)
                                         (CLet 'e-curr
                                               (Local)
                                               (CApp (CId 'next)
                                                     (list (CId 'sub-iter))
                                                     (list)
                                                     (Empty-list))
                                               (CIf (CPrim2 'or
                                                            (CApp (CId 'python-in)
                                                                  (list (CId 'e-curr) (CId 'e-2))
                                                                  (list)
                                                                  (Empty-list))
                                                            (CApp (CId 'python-in)
                                                                  (list (CId 'e-curr) (CId 'e-1))
                                                                  (list)
                                                                  (Empty-list)))
                                                    (CApp (CAttribute 'append (CId 'build-list))
                                                          (list (CId 'e-curr))
                                                          (list)
                                                          (Empty-list))
                                                    (CPass)))
                                         (CPass)
                                         (list))
                                 (list (CExcHandler 'no-name (CId 'StopIteration) (CReturn (CApp (CId 'set)
                                                                                                 (list (CId 'build-list))
                                                                                                 (list)
                                                                                                 (Empty-list)))))
                                 (CPass))))
         (list)
         (list)
         false
         'no-vararg))
;;  (CFunc (list 'e-1 'e-2)
;        (CError (Make-throw 'Exception "Bitwise or not implemented yet!"))
;        (list)
;        (list)
;        'no-vararg))

(define python-bitxor
  (CFunc (list 'e-1 'e-2)
         (CLet 'sub-iter
               (Local)
               (CApp (CId 'iter)
                     (list (CPrim2 'list+ 
                                   (CApp (CId 'list)
                                         (list (CId 'e-1)) 
                                         (list)
                                         (Empty-list))
                                   (CApp (CId 'list)
                                         (list (CId 'e-2))
                                         (list)
                                         (Empty-list))))
                     (list)
                     (Empty-list))
               (CLet 'build-list ;; Needs to be finished... 
                     (Local)
                     (Empty-list)
                     (CTryExcept (CWhile (CTrue)
                                         (CLet 'e-curr
                                               (Local)
                                               (CApp (CId 'next)
                                                     (list (CId 'sub-iter))
                                                     (list)
                                                     (Empty-list))
                                               (CIf (CPrim2 'and 
                                                            (CPrim2 'or
                                                                    (CApp (CId 'python-in)
                                                                          (list (CId 'e-curr) (CId 'e-2))
                                                                          (list)
                                                                          (Empty-list))
                                                                    (CApp (CId 'python-in)
                                                                          (list (CId 'e-curr) (CId 'e-1))
                                                                          (list)
                                                                          (Empty-list)))
                                                            (CPrim1 'not
                                                                    (CPrim2 'and
                                                                            (CApp (CId 'python-in)
                                                                                  (list (CId 'e-curr) (CId 'e-2))
                                                                                  (list)
                                                                                  (Empty-list))
                                                                            (CApp (CId 'python-in)
                                                                                  (list (CId 'e-curr) (CId 'e-1))
                                                                                  (list)
                                                                                  (Empty-list)))))
                                                    (CApp (CAttribute 'append (CId 'build-list))
                                                          (list (CId 'e-curr))
                                                          (list)
                                                          (Empty-list))
                                                    (CPass)))
                                         (CPass)
                                         (list))
                                 (list (CExcHandler 'no-name (CId 'StopIteration) (CReturn (CApp (CId 'set)
                                                                                                 (list (CId 'build-list))
                                                                                                 (list)
                                                                                                 (Empty-list)))))
                                 (CPass))))
         (list)
         (list)
         false
         'no-vararg))
;  (CFunc (list 'e-1 'e-2)
;         (CError (Make-throw 'Exception "Bitwise xor not implemented yet!"))
;         (list)
;         (list)
;         'no-vararg))

(define python-mult ;; eventaully, this has to work for strings and integers too...
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'or
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float")))
              (CIf (CPrim2 'or
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float")))
                   (CPrim2 'num* (CId 'e-1) (CId 'e-2))
                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "bool"))
                        (CPrim2 'num* (CId 'e-1) (CPrim1 'to-int (CId 'e-2)))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "string"))
                             (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                                  (CPrim2 'duplicate (CId 'e-2) (CId 'e-1))
                                  (CError (CStr "*: Cannot multiply string by float type!")))
                             (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "tuple"))
                                  (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                                       (CPrim2 'duple (CId 'e-2) (CId 'e-1))
                                       (CError (CStr "*: Cannot multiply string by float type!")))
                                  (CError (CStr "*: Cannot do math on this type!"))))))
              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))
                   (CIf (CPrim2 'or
                                (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                                (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float")))
                        (CPrim2 'num* (CPrim1 'to-int (CId 'e-1)) (CId 'e-2))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "bool"))
                             (CPrim2 'num* (CPrim1 'to-int (CId 'e-1)) (CPrim1 'to-int (CId 'e-2)))
                             (CError (CStr "*: Cannot do math on this type!"))))
                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                             (CPrim2 'duplicate (CId 'e-1) (CId 'e-2))
                             (CError (CStr "*: Cannot do math on these types... Sorry!")))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "tuple"))
                             (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                                  (CPrim2 'duple (CId 'e-1) (CId 'e-2))
                                  (CError (CStr "*: Cannot do math on these types... Sorry!")))
                             (CError (CStr "*: Cannot do math on this type... Sorry!"))))))
         (list)
         (list)
         false
         'no-vararg))

;; Need to convert this function as well. Divison must handle booleans.
(define python-div
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'and 
                      (CPrim2 'or
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                              (CPrim2 'or
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float"))
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))))
                      (CPrim2 'or
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                              (CPrim2 'or
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float"))
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "bool")))))
              (CIf (CPrim2 'eq (CPrim1 'to-float (CId 'e-2)) (CNum 0.0))
                   (CError (CApp (CId 'ZeroDivisionError)
                                 (list)
                                 (list)
                                 (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list)))))
                   (CPrim2 'num/ (CPrim1 'to-float (CId 'e-1)) (CPrim1 'to-float (CId 'e-2))))
              (CError (CStr "/: Not supported for this type.")))
         (list)
         (list)
         false
         'no-vararg))

;; floor div
(define python-floor-div
  (CFunc (list 'e-1 'e-2)
         (CPrim1 'to-float (CPrim1 'to-int (CApp (CId 'python-div)
                                                 (list (CId 'e-1) (CId 'e-2))
                                                 (list)
                                                 (Empty-list))))
         (list)
         (list)
         false
         'no-vararg))


(define python-mod
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'and
                      (CPrim2 'or 
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float")))
                      (CPrim2 'or 
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float"))))
              (CIf (CPrim2 'eq (CPrim1 'to-float (CId 'e-2)) (CNum 0.0))
                   (CError (CApp (CId 'ZeroDivisionError)
                                 (list (CStr "Tried to mod by zero"))
                                 (list)
                                 (Empty-list)))
                   (CPrim2 'num% (CId 'e-1) (CId 'e-2)))
              (CError (CStr "%: Not supported for this type.")))
         (list)
         (list)
         false
         'no-vararg))




;(define python-div
;  (CFunc (list 'e-1 'e-2)
;         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CPrim1 'tagof (CId 'e-2)))
;              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
;                   (CIf (CPrim2 'eq (CId 'e-2) (CNum 0))
;                        (CError (CStr "/: Divide by zero"))
;                        (CPrim2 'num/ (CId 'e-1) (CId 'e-2)))
;                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float"))
;                        (CIf (CPrim2 'eq (CId 'e-2) (CNum 0))
;                             (CError (CStr "/: Divide by zero"))
;                             (CPrim2 'num/ (CId 'e-1) (CId 'e-2)))
;                        (CError (CStr "/: Not supported for this type."))))
;              (CError (CStr "/: Types do not match.")))
;         (list)))

(define python-lt
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CPrim1 'tagof (CId 'e-2)))
              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                   (CPrim2 'num-lt (CId 'e-1) (CId 'e-2))
                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float"))
                        (CPrim2 'num-lt (CId 'e-1) (CId 'e-2))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))
                             (CPrim2 'string-lt (CId 'e-1) (CId 'e-2))
                             (CError (Make-throw 'TypeError "<: Not supported for this type.")))))
              (CError (Make-throw 'TypeError "<: Types do not match.")))
         (list)
         (list)
         false
         'no-vararg))

(define python-lte
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CPrim1 'tagof (CId 'e-2)))
              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                   (CPrim2 'num-lte (CId 'e-1) (CId 'e-2))
                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float"))
                        (CPrim2 'num-lte (CId 'e-1) (CId 'e-2))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))
                             (CPrim2 'string-lte (CId 'e-1) (CId 'e-2))
                             (CError (Make-throw 'TypeError "<=: Not supported for this type.")))))
              (CError (Make-throw 'TypeError "<=: Types do not match.")))
         (list)
         (list)
         false
         'no-vararg))

(define python-gt
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CPrim1 'tagof (CId 'e-2)))
              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                   (CPrim2 'num-gt (CId 'e-1) (CId 'e-2))
                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float"))
                        (CPrim2 'num-gt (CId 'e-1) (CId 'e-2))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))
                             (CPrim2 'string-gt (CId 'e-1) (CId 'e-2))
                             (CError (Make-throw 'TypeError ">: Not supported for this type.")))))
              (CError (Make-throw 'TypeError ">: Types do not match.")))
         (list)
         (list)
         false
         'no-vararg))

(define python-gte
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CPrim1 'tagof (CId 'e-2)))
              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                   (CPrim2 'num-gte (CId 'e-1) (CId 'e-2))
                   (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float"))
                        (CPrim2 'num-gte (CId 'e-1) (CId 'e-2))
                        (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))
                             (CPrim2 'string-gte (CId 'e-1) (CId 'e-2))
                             (CError (Make-throw 'TypeError ">=: Not supported for this type.")))))
              (CError (Make-throw 'TypeError ">=: Types do not match.")))
         (list)
         (list)
         false
         'no-vararg))

(define python-uadd
  (CFunc (list 'e-1)
         (CIf (CPrim2 'or 
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int")) 
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float")))
              (CId 'e-1)
              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))
                   (CPrim1 'to-int (CId 'e-1))
                   (CError (Make-throw 'TypeError "Unary +: Not supported for this type."))))
         (list)
         (list)
         false
         'no-vararg))

;; TODO: need invert, negate, and not cases. With typechecking. 


(define python-invert
  (CFunc (list 'e-1)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
              (CPrim1 'invert (CId 'e-1))
              (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))
                   (CPrim1 'invert (CPrim1 'to-int (CId 'e-1)))
                   (CError (Make-throw 'TypeError "~: Cannot invert this type."))))
         (list)
         (list)
         false
         'no-vararg))


(define python-eq
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'and 
                      (CPrim2 'or
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                              (CPrim2 'or
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float"))
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))))
                      (CPrim2 'or
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                              (CPrim2 'or
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "float"))
                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "bool")))))
              (CPrim2 'eq (CPrim1 'to-float (CId 'e-1)) (CPrim1 'to-float (CId 'e-2)))
              (CPrim2 'eq (CId 'e-1) (CId 'e-2)))
         (list)
         (list)
         false
         'no-vararg))

(define python-notEq
  (CFunc (list 'e-1 'e-2)
         (CPrim2 'notEq (CId 'e-1) (CId 'e-2))
         (list)
         (list)
         false
         'no-vararg))

(define python-is
  (CFunc (list 'e-1 'e-2)
         (CPrim2 'is (CId 'e-1) (CId 'e-2))
         (list)
         (list)
         false
         'no-vararg))

(define python-isNot
  (CFunc (list 'e-1 'e-2)
         (CPrim1 'not (CPrim2 'is (CId 'e-1) (CId 'e-2)))
         (list)
         (list)
         false
         'no-vararg))

(define python-in
  (CFunc (list 'e-1 'e-2) ;; TODO TODO TODO re-write this to use a for loop for lists and their ilk. 
         (CIf (CPrim2 'or
                      (CPrim2 'or
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "list"))
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "set")))
                      (CPrim2 'or 
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "tuple"))
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "dict")))
                      )
              (CSeq (Create-for-loop 'e-curr 
                                     (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "dict"))
                                          (CApp (CAttribute 'keys (CId 'e-2))
                                                (list)
                                                (list)
                                                (Empty-list))
                                          (CId 'e-2))
                                     (CIf (CPrim2 'eq (CId 'e-curr) (CId 'e-1))
                                          (CReturn (CTrue))
                                          (CPass)))
                    (CReturn (CFalse)))
              (CPrim2 'in (CId 'e-1) (CId 'e-2))) ;; this false branch should become an iterator or recursive function. 
         (list)
         (list)
         false
         'no-vararg))


(define python-notIn
  (CFunc (list 'e-1 'e-2)
         (CPrim1 'not (CApp (CId 'python-in)
                            (list (CId 'e-1) (CId 'e-2))
                            (list)
                            (Empty-list)))
         (list)
         (list)
         false
         'no-vararg))

(define print
  (CFunc (list 'e-1)
         (CPrim1 'print (CApp (CId 'str)
                              (list (CId 'e-1))
                              (list)
                              (Empty-list)))
         (list)
         (list)
         false
         'no-vararg))

(define python-not
  (CFunc (list 'e-1)
         (CPrim1 'not (CId 'e-1))
         (list)
         (list)
         false
         'no-vararg))

(define python-negate
  (CFunc (list 'e-1)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))
              (CPrim1 'negative (CPrim1 'to-int (CId 'e-1)))
              (CIf (CPrim2 'or
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float")))
                   (CPrim1 'negative (CId 'e-1)) ;; can be made much more efficient...
                   (CError (Make-throw 'TypeError "Unary -: Not supported for this type."))))
         (list)
         (list)
         false
         'no-vararg))

(define len 
  (CFunc (list 'e-1)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))
              (CPrim1 'length (CId 'e-1))
              (CIf (CPrim2 'or
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "list"))
                           (CPrim2 'or
                                   (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "_dict"))
                                   (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "tuple"))))
                   (CAttribute '__size__ (CId 'e-1))
                   (CError (Make-throw 'TypeError "len: Argument must be a string, list, tuple or dict (so far...)."))))
         (list)
         (list)
         false
         'no-vararg))

(define abs
  (CFunc (list 'e-1)
         (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))
              (CPrim1 'to-int (CId 'e-1))
              (CIf (CPrim2 'or
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                           (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "float")))
                   (CIf (CPrim2 'num-lt (CId 'e-1) (CNum 0))
                        (CPrim1 'negative (CId 'e-1))
                        (CId 'e-1)) ;; is this the right way to do it?
                   (CError (Make-throw 'TypeError "abs: Argument must be a number or boolean"))))
         (list)
         (list)
         false
         'no-vararg))

;; max
(define python-max
  (CFunc (list 'e-1)
         (CLet 'e-list
               (Local)
               (CApp (CId 'list)
                     (list (CId 'e-1))
                     (list)
                     (Empty-list))
               (CLet 'e-it
                     (Local)
                     (CApp (CId 'iter)
                           (list (CId 'e-list))
                           (list)
                           (Empty-list))
                     (CLet 'e-curr
                           (Local)
                           (CNone)
                           (CTryExcept (CWhile (CTrue) 
                                               (CLet 'e-tmp
                                                     (Local)
                                                     (CApp (CId 'next)
                                                           (list (CId 'e-it))
                                                           (list)
                                                           (Empty-list))
                                                     (CSet (CId 'e-curr) (CIf (CPrim2 'eq (CId 'e-curr) (CNone))
                                                                              (CId 'e-tmp)
                                                                              (CIf (CApp (CId 'python-gt)
                                                                                         (list (CId 'e-tmp) (CId 'e-curr))
                                                                                         (list)
                                                                                         (Empty-list))
                                                                                   (CId 'e-tmp)
                                                                                   (CId 'e-curr))))) 
                                               (CPass) 
                                               (list))
                                       (list (CExcHandler 'no-name (CId 'StopIteration) (CReturn (CId 'e-curr))))
                                       (CPass)))))
         (list)
         (list)
         false
         'no-vararg))



;; min
(define python-min
  (CFunc (list 'e-1)
         (CLet 'e-list
               (Local)
               (CApp (CId 'list)
                     (list (CId 'e-1))
                     (list)
                     (Empty-list))
               (CLet 'e-it
                     (Local)
                     (CApp (CId 'iter)
                           (list (CId 'e-list))
                           (list)
                           (Empty-list))
                     (CLet 'e-curr
                           (Local)
                           (CNone)
                           (CTryExcept (CWhile (CTrue) 
                                               (CLet 'e-tmp
                                                     (Local)
                                                     (CApp (CId 'next)
                                                           (list (CId 'e-it))
                                                           (list)
                                                           (Empty-list))
                                                     (CSet (CId 'e-curr) (CIf (CPrim2 'eq (CId 'e-curr) (CNone))
                                                                              (CId 'e-tmp)
                                                                              (CIf (CApp (CId 'python-lt)
                                                                                         (list (CId 'e-tmp) (CId 'e-curr))
                                                                                         (list)
                                                                                         (Empty-list))
                                                                                   (CId 'e-tmp)
                                                                                   (CId 'e-curr))))) 
                                               (CPass) 
                                               (list))
                                       (list (CExcHandler 'no-name (CId 'StopIteration) (CReturn (CId 'e-curr))))
                                       (CPass)))))
         (list)
         (list)
         false
         'no-vararg))

#|
(CFunc (list 'self 'e-1)
       (CLet 'e-list
             (Local)
             (CApp (CId 'list)
                   (list (CId 'e-1))
                   (list)
                   (Empty-list))
             (CLet 'e-it
                   (Local)
                   (CApp (CId 'iter)
                         (list (Cid 'e-list))
                         (list)
                         (Empty-list))
                   
                   (CTryExcept (CLet 'e-curr
                                     (Local)
                                     (CApp (CId 'next)
                                           (list (CId 'e-it))
                                           (list)
                                           (Empty-list))
                                     (CWhile (CTrue)
                                             (CSet (CSubscript (CId 'self) (CId 'e-curr))
                                                   (CSubscript (CId 'e-list) (CId 'e-curr)))
                                             (CPass)
                                             (list))
                                     (list (CExcHandler 'no-name (CId 'StopIteration) (CReturn (CPass))))
                                     (CPass))))))

   |#

;; Callable
;; may need to re-write this in the future - it depends. I don't know yet. 
(define callable
  (CFunc (list 'e-1)
         (CIf (CPrim2 'or 
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "function"))
                      (CPrim2 'or 
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "class"))
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "primitive-class"))))
              (CTrue)
              (CFalse))
         (list)
         (list)
         false
         'no-vararg))


#|
(define bool ;; needs to handle arbitrary-arity input
  (CFunc (list 'e-1)
         (CPrim1 'to-bool (CId 'e-1))
         (list)
         (list (CFalse))
         'no-vararg))
|#

(define object-class
  (CHash (hash-set (hash-set (hash (list)) 
                             (CStr "__name__") 
                             (CStr "Object")) 
                   (CStr "__init__") 
                   (CFunc (list 'e-1)
                          (CPass)
                          (list)
                          (list (CFalse))
                          false
                          'no-vararg)) 
         (cType "class" (CNone))))

(define bool-primitive-class
  (CHash (hash-set (hash-set (hash (list)) 
                             (CStr "__name__") 
                             (CStr "bool")) 
                   (CStr "__convert__") 
                   (CFunc (list 'e-1)
                          (CPrim1 'to-bool (CId 'e-1))
                          (list)
                          (list (CFalse))
                          false
                          'no-vararg)) 
         (cType "primitive-class" (CId '_Object))))

(define int-primitive-class
  (CHash (hash-set (hash-set (hash (list)) ;; TYPES!
                             (CStr "__name__") 
                             (CStr "int")) 
                   (CStr "__convert__") 
                   (CFunc (list 'e-1)
                          (CPrim1 'to-int (CId 'e-1))
                          (list)
                          (list (CNum 0))
                          false
                          'no-vararg)) 
         (cType "primitive-class" (CId '_Object))))

(define float-primitive-class
  (CHash (hash-set (hash-set (hash (list)) ; TODO TYPE CHECKING!
                             (CStr "__name__") 
                             (CStr "float")) 
                   (CStr "__convert__") 
                   (CFunc (list 'e-1)
                          (CPrim1 'to-float (CId 'e-1))
                          (list)
                          (list (CNum 0.0))
                          false
                          'no-vararg)) 
         (cType "primitive-class" (CId '_Object))))

(define str-primitive-class
  (CHash (hash (list (values (CStr "__name__") (CStr "string"))
                     (values (CStr "__iter__")
                             (CFunc (list 'self)
                                    (CApp (CId 'iter)
                                          (list (CApp (CId 'list)
                                                      (list (CId 'self))
                                                      (list)
                                                      (Empty-list)))
                                          (list)
                                          (Empty-list))
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     (values (CStr "__convert__") 
                             (CFunc (list 'e-1)
                                    (CIf (CPrim2 'has-field (CId 'e-1) (CStr "tostring"))
                                         (CApp (CAttribute 'tostring (CId 'e-1))
                                               (list)
                                               (list)
                                               (Empty-list))
                                         (CPrim1 'to-string (CId 'e-1)))
                                    (list)
                                    (list (CStr ""))
                                    false
                                    'no-vararg)))) 
         (cType "primitive-class" (CId '_Object))))

(define list-primitive-class
  (CHash (hash (list (values (CStr "__size__") (CNum 0))
                     (values (CStr "__getitem__") 
                             (CFunc (list 'self 'e-1)
                                    (CReturn (CSubscript (CId 'self) (CId 'e-1)))
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     (values (CStr "__name__") 
                             (CStr "list")) 
                     (values (CStr "__convert__") 
                             (CFunc (list 'e-1)
                                    (CIf (CPrim2 'or 
                                                 (CPrim2 'or
                                                         (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "set"))
                                                         (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "list")))
                                                 (CPrim2 'or 
                                                         (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "tuple"))
                                                         (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))))
                                         (CPrim1 'to-list (CId 'e-1))
                                         (CIf (CPrim2 'or
                                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "oldIterator"))
                                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "doubleIterator")))
                                              (CLet 'build-list
                                                    (Local)
                                                    (Empty-list)
                                                    (CTryExcept (CWhile (CTrue) 
                                                                        (CSet (CId 'build-list)
                                                                              (CPrim2 'list+ 
                                                                                      (CId 'build-list)
                                                                                      (One-list (CApp (CId 'next)
                                                                                                      (list (CId 'e-1))
                                                                                                      (list)
                                                                                                      (Empty-list))))) 
                                                                        (CPass) 
                                                                        (list))
                                                                (list (CExcHandler 'no-name
                                                                                   (CId 'StopIteration)
                                                                                   (CReturn (CId 'build-list))))
                                                                (CPass)))
                                              (CError (Make-throw 'TypeError "Cannot convert this type to a list")))) ;; TODO be more specific!
                                    (list)
                                    (list (CStr ""))
                                    false
                                    'no-vararg))
                     (values (CStr "append") 
                             (CFunc (list 'self 'e-1)
                                    (CSet (CSubscript (CId 'self) (CApp (CId 'len)
                                                                        (list (CId 'self))
                                                                        (list)
                                                                        (Empty-list))) 
                                          (CId 'e-1))
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     (values (CStr "extend") 
                             (CFunc (list 'self 'e-1)
                                    (Create-for-loop 'e-curr 
                                                     (CId 'e-1) 
                                                     (CApp (CAttribute 'append (CId 'self))
                                                           (list (CId 'e-curr))
                                                           (list)
                                                           (Empty-list)))
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     (values (CStr "tostring") 
                             (CFunc (list 'self)
                                    (CLet 'build-str
                                          (Local)
                                          (CStr "[ ")
                                          (CSeq (Create-for-loop 'e-curr
                                                                 (CId 'self) 
                                                                 (CSet (CId 'build-str)
                                                                       (CPrim2 'string+ 
                                                                               (CId 'build-str) 
                                                                               (CPrim2 'string+
                                                                                       (CApp (CId 'str)
                                                                                             (list (CId 'e-curr))
                                                                                             (list)
                                                                                             (Empty-list))
                                                                                       (CStr " ")))))
                                                (CReturn (CPrim2 'string+ (CId 'build-str) (CStr "]")))))
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     ))
         (cType "primitive-class" (CId '_Object))))


(define tuple-primitive-class
  (CHash (hash (list (values (CStr "__size__") (CNum 0))
                     (values (CStr "__getitem__") 
                             (CFunc (list 'self 'e-1)
                                    (CReturn (CSubscript (CId 'self) (CId 'e-1)))
                                    (list)
                                    (list)
                                    false
                                    'no-vararg) )
                     (values (CStr "__name__") 
                             (CStr "tuple")) 
                     (values (CStr "__convert__") 
                             (CFunc (list 'e-1)
                                    (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "tuple")) 
                                         (CId 'e-1)
                                         (CPrim1 'to-tuple (CId 'e-1)))
                                    (list)
                                    (list (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "tuple" (CId 'tuple))))
                                    false
                                    'no-vararg))
                     (values (CStr "tostring") 
                             (CFunc (list 'self)
                                    (CLet 'build-str
                                          (Local)
                                          (CStr "( ")
                                          (CSeq (Create-for-loop 'e-curr
                                                                 (CId 'self) 
                                                                 (CSet (CId 'build-str)
                                                                       (CPrim2 'string+ 
                                                                               (CId 'build-str) 
                                                                               (CPrim2 'string+
                                                                                       (CApp (CId 'str)
                                                                                             (list (CId 'e-curr))
                                                                                             (list)
                                                                                             (Empty-list))
                                                                                       (CStr " ")))))
                                                (CReturn (CPrim2 'string+ (CId 'build-str) (CStr ")")))))
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     )) 
         (cType "primitive-class" (CId '_Object))))

(define dict-primitive-class
  (CHash (hash (list (values (CStr "__name__") (CStr "_dict"))
                     ;  (values (CStr "__size__") (CNum 0))
                     (values (CStr "get") 
                             (CFunc (list 'self 'e-1 'e-2)
                                    (CTryExcept 
                                     (CReturn (CSubscript (CId 'self) (CId 'e-1))) ;; Check exception...
                                     (list (CExcHandler 'e (CId 'UnboundLocalError) (CId 'e-2)))
                                     (CPass))
                                    (list)
                                    (list (CNone))
                                    false
                                    'no-vararg))
                     (values (CStr "__getitem__") 
                             (CFunc (list 'self 'e-1 'e-2)
                                    (CTryExcept 
                                     (CReturn (CSubscript (CId 'self) (CId 'e-1))) ;; Check exception...
                                     (list (CExcHandler 'e (CId 'UnboundLocalError) (CId 'e-2)))
                                     (CPass))
                                    (list)
                                    (list (CNone))
                                    false
                                    'no-vararg))
                     (values (CStr "__iter__")
                             (CFunc (list 'self)
                                    (CApp (CId 'iter)
                                          (list (CApp (CAttribute 'keys (CId 'self))
                                                      (list)
                                                      (list)
                                                      (Empty-list)))
                                          (list)
                                          (Empty-list))
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     ;;(values (CStr "update") ())
                     (values (CStr "keys") 
                             (CFunc (list 'self)
                                    (CAttribute '__keys__ (CId 'self))
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     (values (CStr "values") 
                             (CFunc (list 'self)
                                    (CLet 'keys-list
                                          (Local)
                                          (CApp (CId 'list)
                                                (list (CAttribute '__keys__ (CId 'self)))
                                                (list)
                                                (Empty-list)) ;; get keys
                                          (CLet 'index-var
                                                (Local)
                                                (CNum 0)
                                                (CLet 'size-var
                                                      (Local)
                                                      (CAttribute '__size__ (CId 'self))
                                                      ;  (CIf (CPrim2 'num-gt (CId 'size-var) (CNum 0))
                                                      (CLet 'build-list
                                                            (Local)
                                                            (Empty-list)
                                                            (CSeq (CWhile (CPrim2 'num-lt 
                                                                                  (CId 'index-var) 
                                                                                  (CId 'size-var))
                                                                          (CSeq (CSet (CId 'build-list)
                                                                                      (CPrim2 'list+ 
                                                                                              (CId 'build-list) 
                                                                                              (One-list 
                                                                                               (CSubscript
                                                                                                (CId 'self)
                                                                                                (CSubscript 
                                                                                                 (CId 'keys-list) 
                                                                                                 (CId 'index-var))))))
                                                                                (CSet (CId 'index-var) 
                                                                                      (CPrim2 'num+ (CId 'index-var) 
                                                                                              (CNum 1))))
                                                                          (CPass)
                                                                          (list))
                                                                  (CReturn (CId 'build-list))))
                                                      ; (Empty-list))
                                                      )))
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     
                     (values (CStr "update") ;; TODO test!
                             (CFunc (list 'self 'e-1)
                                    (CIf (CPrim2 'is (CId 'e-1) (CNone))
                                         (CPass)
                                         (CLet 'e-list
                                               (Local)
                                               (CApp (CId 'list)
                                                     (list (CApp (CAttribute 'keys (CId 'e-1))
                                                                 (list)
                                                                 (list)
                                                                 (Empty-list)))
                                                     (list)
                                                     (Empty-list))
                                               (CLet 'e-it
                                                     (Local)
                                                     (CApp (CId 'iter)
                                                           (list (CId 'e-list))
                                                           (list)
                                                           (Empty-list))
                                                     
                                                     (CTryExcept (CLet 'e-curr
                                                                       (Local)
                                                                       (CApp (CId 'next)
                                                                             (list (CId 'e-it))
                                                                             (list)
                                                                             (Empty-list))
                                                                       (CWhile (CTrue)
                                                                               (CSeq (CSet (CSubscript (CId 'self) (CId 'e-curr))
                                                                                           (CSubscript (CId 'e-1)
                                                                                                       ;(CSubscript (CId 'e-list) 
                                                                                                       (CId 'e-curr)))
                                                                                     (CSet (CId 'e-curr) (CApp (CId 'next)
                                                                                                               (list (CId 'e-it))
                                                                                                               (list)
                                                                                                               (Empty-list))))
                                                                               (CPass)
                                                                               (list)))
                                                                 (list (CExcHandler 'no-name (CId 'StopIteration) (CReturn (CPass))))
                                                                 (CPass)))))
                                    (list)
                                    (list (CNone))
                                    false
                                    'no-vararg))
                     
                     ;; I did this wrong. It needs to create key, value tuples...
                     (values (CStr "items")
                             (CFunc (list 'self)
                                    (CLet 'keys-list
                                          (Local)
                                          (CApp (CId 'list)
                                                (list (CAttribute '__keys__ (CId 'self)))
                                                (list)
                                                (Empty-list)) ;; get keys
                                          (CLet 'index-var
                                                (Local)
                                                (CNum 0)
                                                (CLet 'size-var
                                                      (Local)
                                                      (CAttribute '__size__ (CId 'self))
                                                      (CLet 'build-list
                                                            (Local)
                                                            (Empty-list)
                                                            (CSeq (CWhile (CPrim2 'num-lt 
                                                                                  (CId 'index-var) 
                                                                                  (CId 'size-var))
                                                                          (CSeq (CSet (CId 'build-list)
                                                                                      (CPrim2 'list+
                                                                                              (CId 'build-list)
                                                                                              (One-list
                                                                                               (Make-tuple-pair
                                                                                                (CSubscript 
                                                                                                 (CId 'keys-list) 
                                                                                                 (CId 'index-var))
                                                                                                (CSubscript
                                                                                                 (CId 'self)
                                                                                                 (CSubscript 
                                                                                                  (CId 'keys-list) 
                                                                                                  (CId 'index-var)))))))
                                                                                (CSet (CId 'index-var) 
                                                                                      (CPrim2 'num+ (CId 'index-var) 
                                                                                              (CNum 1))))
                                                                          (CPass)
                                                                          (list))
                                                                  (CReturn (CId 'build-list)))))))
                                    
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     
                     (values (CStr "clear") ;; deletes everything in the dictionary
                             (CFunc (list 'self)
                                    (CLet 'keys-list
                                          (Local)
                                          (CApp (CId 'list)
                                                (list (CAttribute '__keys__ (CId 'self)))
                                                (list)
                                                (Empty-list)) ;; get keys
                                          (CLet 'index-var
                                                (Local)
                                                (CNum 0)
                                                (CLet 'size-var
                                                      (Local)
                                                      (CAttribute '__size__ (CId 'self))
                                                      ;(CIf (CPrim2 'num-gt (CId 'size-var) (CNum 0))
                                                      (CWhile (CPrim2 'num-lt 
                                                                      (CId 'index-var) 
                                                                      (CId 'size-var))
                                                              (CSeq (CDel (list (CSubscript
                                                                                 (CId 'self)
                                                                                 (CSubscript 
                                                                                  (CId 'keys-list) 
                                                                                  (CId 'index-var)))))
                                                                    (CSet (CId 'index-var) 
                                                                          (CPrim2 'num+ (CId 'index-var) 
                                                                                  (CNum 1))))
                                                              (CPass)
                                                              (list))
                                                      ;(CPass)
                                                      )))
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     (values (CStr "tostring") 
                             (CFunc (list 'self)
                                    (CLet 'build-str
                                          (Local)
                                          (CStr "{ ")
                                          (CSeq (Create-for-loop 'e-curr 
                                                                 (CApp (CAttribute 'keys (CId 'self))
                                                                       (list)
                                                                       (list)
                                                                       (Empty-list))
                                                                 (CSet (CId 'build-str)
                                                                       (CPrim2 'string+ 
                                                                               (CId 'build-str) 
                                                                               (CPrim2 'string+
                                                                                       (CPrim2 'string+
                                                                                       (CApp (CId 'str)
                                                                                             (list (CId 'e-curr))
                                                                                             (list)
                                                                                             (Empty-list))
                                                                                       (CStr ":"))
                                                                                       (CPrim2 'string+
                                                                                       (CApp (CId 'str)
                                                                                             (list (CSubscript (CId 'self) 
                                                                                                               (CId 'e-curr)))
                                                                                             (list)
                                                                                             (Empty-list))
                                                                                       (CStr " ")))
                                                                                       )))
                                                (CReturn (CPrim2 'string+ (CId 'build-str) (CStr "}")))))
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     
                     ;     (values (CStr "update") ;; updates dictionary by adding contents of other dictionary
                     ;            ())
                     ))
         (cType "primitive-class" (CId '_Object)))) ;; If we need a __convert__ method, we'll write one later. 

;; primitive class for sets
(define set-primitive-class
  (CHash (hash (list (values (CStr "__name__") (CStr "set"))
                     (values (CStr "__iter__")
                             (CFunc (list 'self)
                                    (CApp (CId 'iter)
                                          (list (CApp (CId 'list)
                                                      (list (CId 'self))
                                                      (list)
                                                      (Empty-list)))
                                          (list)
                                          (Empty-list))
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     (values (CStr "__convert__") 
                             (CFunc (list 'e-1)
                                    (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "set"))
                                         (CId 'e-1)
                                         (CIf (CPrim2 'or
                                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "list"))
                                                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "tuple")))
                                              (CPrim1 'to-set (CId 'e-1))
                                              (CError (Make-throw 'TypeError "Cannot convert this type to a set"))))
                                    (list)
                                    (list (Empty-list))
                                    false
                                    'no-vararg))
                     (values (CStr "tostring") 
                             (CFunc (list 'self)
                                    (CLet 'build-str
                                          (Local)
                                          (CStr "{ ")
                                          (CSeq (Create-for-loop 'e-curr 
                                                                 (CId 'self) 
                                                                 (CSet (CId 'build-str)
                                                                       (CPrim2 'string+ 
                                                                               (CId 'build-str) 
                                                                               (CPrim2 'string+
                                                                                       (CApp (CId 'str)
                                                                                             (list (CId 'e-curr))
                                                                                             (list)
                                                                                             (Empty-list))
                                                                                       (CStr " ")))))
                                                (CReturn (CPrim2 'string+ (CId 'build-str) (CStr "}")))))
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     ))
         (cType "primitive-class" (CId '_Object))))


;(define recursive-values-help
;  (CLet 'recursive-values-help
;        (Local)
;        (CFunc (list) (CError (CStr "Dummy! (python-make-range)")) (list) (list) 'no-vararg)
;        (CSet (CId 'recursive-values-help)
;              (CFunc ()
;                     ()
;                     ()
;                     ()
;                     'no-vararg))))


;; this will be the builtin class for old iterators
(define python-oldIterator-class
  (CHash (hash (list (values (CStr "__name__") (CStr "oldIterator"))
                     (values (CStr "__init__")
                             (CFunc (list 'self 'anObject) ;2 arguments: self and 'anObject'
                                    (CSeq (CSet (CAttribute 'n (CId 'self)) (CNum 0))
                                          (CSet (CAttribute 'obj (CId 'self)) (CIf (CPrim2 'eq (CPrim1 'tagof (CId 'anObject)) (CStr "list"))
                                                                                   (CApp (CId 'list)
                                                                                         (list (CId 'anObject))
                                                                                         (list)
                                                                                         (Empty-list))
                                                                                   (CId 'anObject)))) ;set self.n=0 and self.obj=anObject
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     (values (CStr "__next__")
                             (CFunc (list 'self) ; 1 argument: self
                                    (CTryExcept (CLet '_i
                                                      (Local)
                                                      (CApp (CAttribute '__getitem__ (CAttribute 'obj (CId 'self)))
                                                            (list (CAttribute 'n (CId 'self)))
                                                            (list)
                                                            (Empty-list))
                                                      (CSeq (CSet (CAttribute 'n (CId 'self)) 
                                                                  (CApp (CId 'python-add)
                                                                        (list (CAttribute 'n (CId 'self)) (CNum 1))
                                                                        (list)
                                                                        (Empty-list))) 
                                                            (CReturn (CId '_i))))
                                                (list (CExcHandler 'no-name 
                                                                   (CId 'IndexError)
                                                                   (CError (CApp (CId 'StopIteration)
                                                                                 (list)
                                                                                 (list)
                                                                                 (Empty-list))))) ;; TODO except case
                                                (CPass)) ;try: 
                                    ;     i = self.obj.__getItem__(self.n)
                                    ;     self.n+=1
                                    ;     return i
                                    ;except IndexError:
                                    ;     raise StopIteration
                                    (list)
                                    (list) ;; default args? 
                                    false
                                    'no-vararg))
                     (values (CStr "__iter__")
                             (CFunc (list 'self) ;1 argument: self
                                    (CReturn (CId 'self)) ;return self
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     ))
         (cType "class" (CId '_Object))))



;; this will be the builtin class for iterators that have a function and a value
(define python-doubleIterator-class
  (CHash (hash (list (values (CStr "__name__") (CStr "doubleIterator"))
                     (values (CStr "__init__")
                             (CFunc (list 'self 'func 'value); 3 arguments: self, func and value
                                    (CSeq (CSet (CAttribute 'func (CId 'self)) (CId 'func))
                                          (CSeq (CSet (CAttribute 'val (CId 'self)) (CId 'value))
                                                (CSet (CAttribute '_i (CId 'self)) (CNone)))) ;set self.func=func and self.val=value
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     (values (CStr "__next__")
                             (CFunc (list 'self) ; 1 argument: self
                                    (CSeq (CIf (CPrim2 'eq (CAttribute '_i (CId 'self)) (CAttribute 'val (CId 'self)))
                                               (CError (CApp (CId 'StopIteration)
                                                             (list)
                                                             (list)
                                                             (Empty-list)))
                                               (CSet (CAttribute '_i (CId 'self))
                                                     (CLet 'funcHelper
                                                           (Local)
                                                           (CAttribute 'func (CId 'self))
                                                           (CApp (CId 'funcHelper)
                                                                 (list)
                                                                 (list)
                                                                 (Empty-list)))))
                                          (CIf (CPrim2 'eq (CAttribute '_i (CId 'self)) (CAttribute 'val (CId 'self)))
                                               (CError (CApp (CId 'StopIteration)
                                                             (list)
                                                             (list)
                                                             (Empty-list)))
                                               (CReturn (CAttribute '_i (CId 'self)))))
                                    ;     i = self.func()
                                    ;     if (i==value) then raise StopIteration else return i
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     #|
                     (values (CStr "__next__")
                             (CFunc (list 'self) ; 1 argument: self
                                    (CLet '_i
                                          (Local)
                                          (CApp (CAttribute 'func (CId 'self))
                                                (list)
                                                (list)
                                                (Empty-list))
                                          (CIf (CPrim2 'eq (CId '_i) (CAttribute 'val (CId 'self)))
                                               (CError (CApp (CId 'StopIteration)
                                                             (list)
                                                             (list)
                                                             (Empty-list)))
                                                      (CReturn (CId '_i))))
                                    ;     i = self.func()
                                    ;     if (i==value) then raise StopIteration else return i
                                    (list)
                                    (list)
                                    'no-vararg))
|#         
                     (values (CStr "__iter__")
                             (CFunc (list 'self) ;1 argument: self
                                    (CReturn (CId 'self)) ;return self
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     ))
         (cType "class" (CId '_Object))))


(define call-iter
  (CFunc (list 'e-1 'e-2)
         (CIf (CPrim2 'is (CId 'e-2) (CNone))
              ;; when iter is called with just one argument
              (CLet 'e-one
                    (Local)
                    (CIf (CPrim2 'or 
                                 (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "string"))
                                 (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "set")))
                         (CApp (CId 'list)
                               (list (CId 'e-1))
                               (list)
                               (Empty-list))
                         (CId 'e-1))
                    (CIf (CPrim2 'has-field (CId 'e-one) (CStr "__iter__"))
                         ;;IF ('E-1 IS A TRING)
                         ;;   (CALL ITER(LIST('E-1)))
                         ;;   (ELSE, CONTINUE WITH WHAT'S BELOW)
                         
                         (CReturn (CApp  (CAttribute '__iter__ (CId 'e-one))
                                         (list)
                                         (list)
                                         (Empty-list)));'e-1.__iter__())
                         (CIf (CPrim2 'has-field (CId 'e-one) (CStr "__getitem__"));(has attribute __getItem__)
                              (CReturn (CApp (CId '_oldIterator)
                                             (list (CId 'e-one))
                                             (list)
                                             (Empty-list)))
                              (CError (Make-throw 'TypeError "Object lacks __getitem__ field, and thus can't be made into oldIterator")))))
              ;; when iter is called with two arguments
              (CReturn (CApp (CId '_doubleIterator)
                             (list (CId 'e-1) (CId 'e-2))
                             (list)
                             (Empty-list))));(call _doubleIterator('e-1,'e-2)))
         (list)
         (list (CNone))
         false
         'no-vararg))



(define call-next
  (CFunc (list 'e-1)
         ;'e-1.next
         (CApp (CAttribute '__next__ (CId 'e-1))
               (list)
               (list)
               (Empty-list))
         (list)
         (list)
         false
         'no-vararg))


;;locals() method
(define python-locals
  (CFunc (list)
         (CPrim1 '_locals (CNone))
         (list)
         (list)
         false
         'no-vararg))

;; make slice...
(define make-slice
  (CFunc (list 'e-list 'e-lower 'e-upper 'e-step)
         (CLet 'e-len
               (Local)
               (CApp (CId 'len)
                     (list (CId 'e-list))
                     (list)
                     (Empty-list))
               (CLet 'e-sign
                     (Local)
                     (CApp (CId 'python-lt)
                           (list (CId 'e-step) (CNum 0))
                           (list)
                           (Empty-list))
                     (CLet 'e-low
                           (Local)
                           (CIf (CPrim2 'eq (CId 'e-lower) (CNone))
                                (CIf (CId 'e-sign)
                                     (CApp (CId 'python-sub)
                                           (list (CId 'e-len) (CNum 1))
                                           (list)
                                           (Empty-list))
                                     (CNum 0))
                                (CIf (CId 'e-sign)
                                     (CIf (CApp (CId 'python-gte)
                                                (list (CId 'e-lower) (CId 'e-len))
                                                (list)
                                                (Empty-list))
                                          (CApp (CId 'python-sub)
                                                (list (CId 'e-len) (CNum 1))
                                                (list)
                                                (Empty-list))
                                          (CId 'e-lower))
                                     (CIf (CApp (CId 'python-lt)
                                                (list (CId 'e-lower) (CNum 0))
                                                (list)
                                                (Empty-list))
                                          (CNum 0)
                                          (CId 'e-lower))))
                           (CLet 'e-up
                                 (Local)
                                 (CIf (CPrim2 'eq (CId 'e-upper) (CNone))
                                      (CIf (CId 'e-sign)
                                           (CNum -1)
                                           (CId 'e-len))
                                      (CIf (CId 'e-sign)
                                           (CIf (CApp (CId 'python-lt)
                                                      (list (CId 'e-upper) (CNum 0))
                                                      (list)
                                                      (Empty-list))
                                                (CNum -1)
                                                (CId 'e-upper))
                                           (CIf (CApp (CId 'python-gte)
                                                      (list (CId 'e-upper) (CId 'e-len))
                                                      (list)
                                                      (Empty-list))
                                                (CId 'e-len)
                                                (CId 'e-upper))))
                                 (CLet 'build-string
                                       (Local)
                                       (CStr "")
                                       (CSeq (Create-for-loop 'e-curr 
                                                              ;(CIf (CId 'e-sign)
                                                              ;     (CApp (CId 'range)
                                                              ;           (list (CApp (CId 'python-sub)
                                                              ;                       (list (CId 'e-up) (CNum 1))
                                                              ;                       (list)
                                                              ;                       (Empty-list))
                                                              ;                 (CApp (CId 'python-sub)
                                                              ;                       (list (CId 'e-low) (CNum 1))
                                                              ;                       (list)
                                                              ;                       (Empty-list)) 
                                                              ;                 (CId 'e-step))
                                                              ;           (list)
                                                              ;           (Empty-list))
                                                              (CApp (CId 'range)
                                                                    (list (CId 'e-low) (CId 'e-up) (CId 'e-step))
                                                                    (list)
                                                                    (Empty-list))
                                                              ;) 
                                                              (CSet (CId 'build-string)
                                                                    (CPrim2 'string+
                                                                            (CId 'build-string)
                                                                            (CSubscript (CId 'e-list) (CId 'e-curr)))))
                                             (CReturn (CId 'build-string))))))))
         
         (list)
         (list)
         false
         'no-vararg))


(define make-range
  (CFunc (list 'e-1 'e-2 'e-3)
         (CIf (CPrim2 'and
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                      (CPrim2 'and 
                              (CPrim2 'is (CId 'e-2) (CNone)) 
                              (CPrim2 'is (CId 'e-3) (CNone))))
              (CApp (CId 'python-make-range) 
                    (list (CNum 0) (CId 'e-1) (CNum 1)) 
                    (list)
                    (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list)))
                    )
              (CIf (CPrim2 'and
                           (CPrim2 'is (CId 'e-3) (CNone))
                           (CPrim2 'and
                                   (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                                   (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))))
                   (CApp (CId 'python-make-range) 
                         (list (CId 'e-1) (CId 'e-2) (CNum 1)) 
                         (list)
                         (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list)))
                         )
                   (CIf (CPrim2 'and 
                                (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "int"))
                                (CPrim2 'and
                                        (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "int"))
                                        (CPrim2 'eq (CPrim1 'tagof (CId 'e-3)) (CStr "int"))))
                        (CIf (CPrim1 'not (CPrim2 'eq (CId 'e-3) (CNum 0)))
                             (CApp (CId 'python-make-range) 
                                   (list (CId 'e-1) (CId 'e-2) (CId 'e-3)) 
                                   (list)
                                   (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list)))
                                   )
                             (CError (CApp (CId 'ValueError)
                                           (list)
                                           (list)
                                           (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list))))))
                        (CApp (CId 'TypeError)
                              (list)
                              (list)
                              (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list))))))) 
         (list)
         (list (CNone) (CNone))
         false
         'no-vararg))

;(CPrim2 'num* 
;        (CId 'e-2)
;        (CPrim2 'num/ 
;                (CId 'e-3) 
;                (CApp (CId 'abs)
;                      (list (CId 'e-3))
;                      (list)
;                      (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list))))))


(define python-make-range
  (CLet 'python-make-range
        (Local)
        (CFunc (list) (CError (CStr "Dummy! (python-make-range)")) (list) (list) false 'no-vararg)
        (CSet (CId 'python-make-range)
              (CFunc (list 'e-1 'e-2 'e-3)
                     (CIf (CPrim2 'num-gte 
                                  (CPrim2 'num* 
                                          (CId 'e-1)
                                          (CPrim2 'num/ 
                                                  (CId 'e-3) 
                                                  (CApp (CId 'abs)
                                                        (list (CId 'e-3))
                                                        (list)
                                                        (Empty-list)))) 
                                  (CPrim2 'num* 
                                          (CId 'e-2)
                                          (CPrim2 'num/ 
                                                  (CId 'e-3) 
                                                  (CApp (CId 'abs)
                                                        (list (CId 'e-3))
                                                        (list)
                                                        (Empty-list)))))
                          (Empty-list)
                          (CPrim2 'list+ 
                                  (One-list (CId 'e-1))
                                  (CApp (CId 'python-make-range) 
                                        (list (CPrim2 'num+ (CId 'e-1) (CId 'e-3)) (CId 'e-2) (CId 'e-3))
                                        (list)
                                        (Empty-list)
                                        )))
                     (list)
                     (list)
                     false
                     'no-vararg))))


;; any
(define python-any
  (CFunc (list 'e-1)
         (CLet '_x
               (Local)
               (CUnbound)
               (desugar (PySeq
                         (list (PyFor (PyId '_x) 
                                      (PyId 'e-1)
                                      (PyIf (PyApp (PyId 'bool)
                                                   (list (PyId '_x))
                                                   (list)
                                                   (PyHolder (Empty-list)))
                                            (list (PyReturn (PyHolder (CTrue))))
                                            (list (PyPass))))
                               (PyReturn (PyHolder (CFalse)))))))
         (list)
         (list)
         false
         'no-vararg
         ))


;; all
(define python-all
  (CFunc (list 'e-1)
         (CLet '_x
               (Local)
               (CUnbound)
               (desugar (PySeq
                         (list (PyFor (PyId '_x) 
                                      (PyId 'e-1)
                                      (PyIf (PyApp (PyId 'bool)
                                                   (list (PyId '_x))
                                                   (list)
                                                   (PyHolder (Empty-list)))
                                            (list (PyPass))
                                            (list (PyReturn (PyHolder (CFalse))))))
                               (PyReturn (PyHolder (CTrue)))))))
         (list)
         (list)
         false
         'no-vararg
         ))

;; filter
(define python-filter
  (CFunc (list 'e-1 'e-2)
         (CLet '_lst
               (Local)
               (CApp (CId 'list)
                     (list)
                     (list)
                     (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list))))
               (CLet '_x
                     (Local)
                     (CUnbound)
                     (CSeq (desugar (PyFor (PyId '_x) 
                                           (PyId 'e-2)
                                           (PyIf (PyApp (PyIf (PyHolder (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "NoneType")))
                                                              (list (PyId 'bool))
                                                              (list (PyId 'e-1)))
                                                        (list (PyId '_x))
                                                        (list)
                                                        (PyHolder (Empty-list)))
                                                 (list (PyApp (PyAttribute 'append (PyId '_lst)) ;the _lst identifier comes from the declared list in the [PyListComp] case in the desugarer
                                                              (list (PyId '_x))
                                                              (list)
                                                              (PyHolder (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list))))))
                                                 (list (PyPass)))
                                           ))
                           (CId '_lst))))
         
         (list)
         (list)
         false
         'no-vararg))


;;helper for any and all and filter
(define python-iter-help
  (CLet 'python-iter-help
        (Local)
        (CFunc (list) (CError (CStr "Dummy! (python-iter-help)")) (list) (list) false 'no-vararg)
        (CSet (CId 'python-iter-help)
              (CFunc (list 'e-list 'e-test 'e-index)
                     (CIf (CApp (CId 'python-lt)
                                (list (CId 'e-index) (CApp (CId 'len)
                                                           (list (CId 'e-list))
                                                           (list)
                                                           (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list)))))
                                (list)
                                (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list))))
                          (CIf (CApp (CId 'e-test) ;; TODO might want to check the type here. Or just in interpreter...
                                     (list (CSubscript (CId 'e-list) (CId 'e-index))) ;; check subscript 'e-index of list
                                     (list)
                                     (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list))))
                               (CPrim2 'list+ ;; check for subscript on next line as well...
                                       (CHash (hash-set (hash (list (values (CStr "__size__") (CNum 1)))) (CNum 0) (CSubscript (CId 'e-list) (CId 'e-index))) (cType "list" (CId 'list))) 
                                       (CApp (CId 'python-iter-help)
                                             (list (CId 'e-list) (CId 'e-test) (CPrim2 'num+ (CId 'e-index) (CNum 1)))
                                             (list)
                                             (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list)))))
                               (CApp (CId 'python-iter-help)
                                     (list (CId 'e-list) (CId 'e-test) (CPrim2 'num+ (CId 'e-index) (CNum 1)))
                                     (list)
                                     (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list)))))
                          (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list))))
                     (list)
                     (list)
                     false
                     'no-vararg))))


;; TODO write min and max







(define python-isinstance
  (CFunc (list 'e-1 'e-2) ;; TODO THIS HAS NO INHERITENCE! We'll want to return a list of inherited classes, and check membership...
         (CIf (CPrim2 'or
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "class"))
                      (CPrim2 'eq (CPrim1 'tagof (CId 'e-2)) (CStr "primitive-class")))
              (CPrim2 'or
                      (CPrim2 'isinstance (CId 'e-1) (CAttribute '__name__ (CId 'e-2)))
                      (CPrim2 'and
                              (CPrim2 'eq (CPrim1 'tagof (CId 'e-1)) (CStr "bool"))
                              (CPrim2 'eq (CAttribute '__name__ (CId 'e-2)) (CStr "int"))))
              (CError (CApp (CId 'TypeError)
                            (list)
                            (list)
                            (CHash (hash (list (values (CStr "__size__") (CNum 0)))) (cType "list" (CId 'list))))))
         (list)
         (list)
         false
         'no-vararg))


(define create-global-env
  (CFunc (list)
         (CGlobalEnv)
         (list)
         (list)
         false
         'no-vararg))


(define true-val
  (CTrue))

(define python-fail
  (CFunc (list 'e-1)
         (CError (CApp (CId 'Exception)
                       (list (CId 'e-1))
                       (list)
                       (Empty-list)))
         (list)
         (list)
         false
         'no-vararg))



;; TODO TODO TODO update these to work like the final classs system...
(define type-error-def
  ;(CClass (hash (list)) (Type "TypeError" (VNone)))) ;; TEMPORARY TypeError definition...
  (CHash (hash (list (values (CStr "__name__") (CStr "TypeError")))) (cType "class" (CId 'Exception))))

(define index-error-def
  ;(CClass (hash (list)) (Type "IndexError" (VNone))))
  (CHash (hash (list (values (CStr "__name__") (CStr "IndexError")))) (cType "class" (CId 'Exception))))

(define zero-division-error
  ;(CClass (hash (list)) (Type "ZeroDivisonError" (VNone))))
  (CHash (hash-set (hash (list)) (CStr "__name__") (CStr "ZeroDivisionError")) (cType "class" (CId 'Exception))))

(define key-error
  ;(CClass (hash (list)) (Type "KeyError" (VNone))))
  (CHash (hash (list (values (CStr "__name__") (CStr "KeyError")))) (cType "class" (CId 'Exception))))

(define runtime-error
  ;(CClass (hash (list)) (Type "RuntimeError" (VNone))))
  (CHash (hash (list (values (CStr "__name__") (CStr "RuntimeError"))
                     (values (CStr "__init__") 
                             (CFunc (list 'self 'e-1)
                                    (CSet (CAttribute 'message (CId 'self)) (CId 'e-1))
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     (values (CStr "message") (CStr "RuntimeError"))
                     )) 
         (cType "class" (CId 'Exception))))

(define unbound-local-error
  ;(CClass (hash (list)) (Type "UnboundLocalError" (VNone))))
  (CHash (hash (list (values (CStr "__name__") (CStr "UnboundLocalError")))) (cType "class" (CId 'Exception))))

(define name-error
  ;(CClass (hash (list)) (Type "NameError" (VNone))))
  (CHash (hash (list (values (CStr "__name__") (CStr "NameError")))) (cType "class" (CId 'Exception))))

(define value-error
  (CHash (hash (list (values (CStr "__name__") (CStr "ValueError")))) (cType "class" (CId 'Exception))))

(define stop-iteration
  (CHash (hash (list (values (CStr "__name__") (CStr "StopIteration")))) (cType "class" (CId 'Exception))))

(define attribute-error
  (CHash (hash (list (values (CStr "__name__") (CStr "AttributeError")))) (cType "class" (CId 'Exception))))

(define assertion-error
  (CHash (hash (list (values (CStr "__name__") (CStr "AssertionError")))) (cType "class" (CId 'Exception))))

(define Exception
  ;(CClass (hash (list)) (Type "Exception" (CNone))))
  (CHash (hash (list (values (CStr "__name__") (CStr "Exception"))
                     (values (CStr "message") (CStr "Exception"))
                     (values (CStr "tostring") 
                             (CFunc (list 'self)
                                    (CPrim2 'string+
                                            (CAttribute '__name__ (CId 'self))
                                            (CPrim2 'string+
                                                    (CStr ": ")
                                                    (CApp (CId 'str)
                                                          (list (CAttribute 'message (CId 'self)))
                                                          (list)
                                                          (Empty-list))))
                                    (list)
                                    (list)
                                    false
                                    'no-vararg))
                     (values (CStr "__init__")
                             (CFunc (list 'self 'e-1)
                                    (CSet (CAttribute 'message (CId 'self)) (CId 'e-1))
                                    (list)
                                    (list (CNone))
                                    false
                                    'no-vararg))
                     )) 
         (cType "class" (CId '_Object))))

(define stop-iteration-error
  (CHash (hash (list (values (CStr "__name__") (CStr "StopIterationError")))) (cType "class" (CId 'Exception))))


(define-type LibBinding
  [bind (left : symbol) (right : CExp)])

;; Here, we define built-in CLASSES


(define TestClass
  (CHash (hash (list (values (CStr "f") (CFunc (list)
                                               (CPrim1 'print (CStr "An Exception has been thrown. "))
                                               (list)
                                               (list)
                                               false
                                               'no-vararg))
                     ;(hash (list)) (list) 'no-vararg (CPrim1 'print (CStr "printing")) (list) -1))
                     (values (CStr "__name__") (CStr "Exception")))) (cType "class" (CId '_Object))))

(define testObj
  (CHash (hash (list)) (cType "Object" (CId 'TestClass))))

;;STILL TO DO: assertRaises and fail
(define lib-functions
  (list (bind 'print print-lambda)
        (bind 'True true-val)
        (bind '___assertTrue assert-true-lambda)
        (bind '___assertFalse assert-false-lambda)
        (bind '___assertIn assert-in-lambda)
        (bind '___assertNotIn assert-notIn-lambda)
        (bind '___assertEqual assert-equal-lambda)
        (bind '___assertNotEqual assert-notEqual-lambda)
        (bind '___assertIs assert-is-lambda)
        (bind '___assertIsNot assert-isNot-lambda)
        (bind '___assertRaises assert-raises-lambda)
        (bind '___fail python-fail)
        (bind 'python-add python-add)
        (bind 'subtract-sets-helper subtract-sets-helper) ;; helper method for subtract
        (bind 'python-sub python-sub)
        (bind 'python-mult python-mult)
        (bind 'python-div python-div)
        (bind 'python-floor-div python-floor-div)
        (bind 'python-mod python-mod)
        (bind 'python-lt python-lt)
        (bind 'python-lte python-lte)
        (bind 'python-gt python-gt)
        (bind 'python-gte python-gte)
        (bind 'python-eq python-eq)
        (bind 'python-notEq python-notEq)
        (bind 'python-is python-is)
        (bind 'python-isNot python-isNot)
        (bind 'python-in python-in)
        (bind 'python-notIn python-notIn)
        (bind 'python-bitand python-bitand)
        (bind 'python-bitor python-bitor)
        (bind 'python-bitxor python-bitxor)
        (bind 'len len)
        (bind 'abs abs)
        ; bind '_Object
        (bind '_Object object-class)
        ; (bind 'bool bool)
        (bind 'bool bool-primitive-class)
        ; (bind 'str str)
        (bind 'str str-primitive-class)
        ; (bind 'float float)
        (bind 'float float-primitive-class)
        ; (bind 'int int)
        (bind 'int int-primitive-class)
        ; (bind 'list make-list)
        (bind 'list list-primitive-class)
        ; (bind 'tuple make-tuple)
        (bind 'tuple (CNone))
        (bind 'tuple tuple-primitive-class)
        (bind '_dict dict-primitive-class)
        (bind 'set set-primitive-class)
        (bind 'callable callable)
        (bind 'range make-range)
        (bind 'python-make-range python-make-range)
        (bind 'python-uadd python-uadd)
        (bind 'python-invert python-invert)
        (bind 'print print)
        (bind 'python-not python-not)
        (bind 'python-negate python-negate)
        (bind 'True (CTrue)) ;; not entirely sure these should be here, but we're passing more tests now...
        (bind 'False (CFalse))
        (bind 'create-global-env create-global-env)
        (bind 'isinstance python-isinstance)
        (bind 'python-iter-help python-iter-help)
        (bind 'make-slice make-slice)
        (bind 'filter python-filter)
        (bind '_oldIterator python-oldIterator-class)
        (bind '_doubleIterator python-doubleIterator-class)
        (bind 'varLocals (Empty-list))
        (bind 'locals python-locals)
        ;  (bind 'iter (CNone)) ;; helps with recursion...
        (bind 'iter call-iter)
        (bind 'next call-next)
        
        (bind 'min python-min)
        (bind 'max python-max)
        (bind 'any python-any)
        (bind 'all python-all)
        
        ;; iterator functions and classes
        ;   (bind 'next call-next)
        ;   (bind 'iter call-iter)
        ;  (bind 'oldIterator python-oldIterator-class)
        ;  (bind 'doubleIterator python-doubleIterator-class)
        
        ;; exceptions (prelim...)
        (bind 'Exception Exception)
        (bind 'TypeError type-error-def)
        (bind 'IndexError index-error-def)
        (bind 'ZeroDivisionError zero-division-error)
        (bind 'KeyError key-error)
        (bind 'RuntimeError runtime-error)
        (bind 'UnboundLocalError unbound-local-error)
        (bind 'NameError name-error)
        (bind 'ValueError value-error)
        (bind 'StopIteration stop-iteration)
        (bind 'AttributeError attribute-error)
        (bind 'AssertionError assertion-error)
        
        ;;binding of built-in classes
        
        (bind 'TestClass TestClass)
        
        
        
        ;; Some more permanent builtin classes
        ;;  (bind 'ZeroDivisionError zero-division-error)
        
        ;;object for debugging
        (bind 'testObj testObj)
        
        ))


(define (python-lib expr)
  (local [(define (python-lib/recur libs)
            (cond [(empty? libs) expr]
                  [(cons? libs)
                   (type-case LibBinding (first libs)
                     (bind (name value)
                           (CLet name (Local) value
                                 (python-lib/recur (rest libs)))))]))]
    (python-lib/recur lib-functions)))


