#lang plai-typed/untyped

(require [typed-in racket (format : (string 'a -> string))])
;;; flags can control which Lex* will be desugared.

;; this stores the str version of the flag and its modification function
;; (hashof str * (bool -> void))
(define _inner_flags_hash (make-hash empty))

;; set-flag will take the flag's str name and set the flag to v
;; set-flag: str * bool -> void
(define (set-flag flag-str v)
  (begin
    (let ((func (hash-ref _inner_flags_hash flag-str)))
      (type-case (optionof 'a) func
        [none () (error 'set-flag (format "flag ~a not found" flag-str))]
        [some (fun) (fun v)]))))

;; define-flag will define a flag and push a corresponding
;; modification function into _inner_flags_hash
(define-syntax (define-flag stx)
  (syntax-case stx ()
    [(_ flag flag-modifier)
     (with-syntax ([flag-str (datum->syntax 
                              stx 
                              (format "~a" (syntax->datum #'flag)))])
                                                        
       #'(define flag
           (begin (hash-set! flag-modifier flag-str (lambda (v) (set! flag v)))
                  true)))]
    [(_ flag)
     #'(define-flag flag _inner_flags_hash)]))



;;;========= flags ===========

;; flags for assignment
(define-flag dsg-subscript-assignment)
(define-flag dsg-tuple-assignment)
;; flags for func
(define-flag dsg-func-kwonlyargs)
(define-flag dsg-func-kwarg)
;; flags for built-in primes
(define-flag dsg-built-in-prims)
;; flags for for statement
(define-flag dsg-for)
;; flags for exception, try, with statement
(define-flag dsg-with)
(define-flag dsg-try-finally)
(define-flag dsg-try-except-else)
;; flags for function exec. Check the util.rkt
;; TODO: try to desugar LexApp to LexPass(an empty lambda)
(define-flag dsg-app)
;; flags for subscript
(define-flag dsg-subscript)
;; flags for get object fields
(define-flag dsg-dot-field)
;; flags for while statement
(define-flag dsg-while)

;; flags for delete
(define-flag dsg-delete)
;; flags for augmented assignment
(define-flag dsg-augassignment)
;; flags for set attribute
(define-flag dsg-expr-assign)
;; flags for get attribute
(define-flag dsg-expr-field)
;; flags for class: TODO: need more specific?
(define-flag dsg-class)
;; flags for tuple
(define-flag dsg-tuple)
;; flags for list
(define-flag dsg-list)
;; flags for set(useless)
;; (define-flag dsg-set)
;; flags for dict
(define-flag dsg-dict)
;; flags for lambda
(define-flag dsg-lam) ; TODO: LexLam has many arguments
;; flags for assert
(define-flag dsg-assert) ; TODO: what to do with this?
;; flags for pass: this is interesting--how important the pass statement in Python?
;; no idea how to write the code
;; (define-flag dsg-pass)
;; flags for raise
(define-flag dsg-raise)
;; flags for localLet and global let
(define-flag dsg-locallet) ; TODO: what to do with this?
(define-flag dsg-globallet); TODO: what to do with this?


;; may be useful
(define-flag dsg-listcomp)
(define-flag dsg-compop)
(define-flag dsg-boolop)
(define-flag dsg-unaryop)
(define-flag dsg-binop)



; No desugaring: LexReturn, LexBreak, LexContinue, 

;; TODO: decorator
