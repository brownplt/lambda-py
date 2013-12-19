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
(define-flag dsg-multiple-assignment)
;; flags for subscript
(define-flag dsg-subscript)
;; flags for raise
(define-flag dsg-raise)
;; flags for star args
(define-flag dsg-function-arguments)
(define-flag dsg-function-starargs)

(define-flag dsg-callable-runtime-checking)

;; if dsg-decorator is false, no class decorator will be desugared.
(define-flag dsg-decorator)
;; if dsg-metaclass is false, 
(define-flag dsg-metaclass)

(define-flag dsg-multiple-inheritance)

;; flags for pass: this is interesting--how important the pass statement in Python?
;; no idea how to write the code
;; (define-flag dsg-pass)





;; TODO: decorator
