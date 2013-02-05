#lang plai-typed

(require "python-core-syntax.rkt"
         "python-phase1.rkt"
         "python-desugar.rkt"
         "util.rkt"
         (typed-in "parse-python.rkt" (parse-python/port : ('a 'b -> 'c)))
         (typed-in "get-structured-python.rkt"
                   (get-structured-python : ('a -> 'b)))   
         (typed-in racket/base (open-input-file : ('a -> 'b)))
         (typed-in racket/base (open-input-string : ('a -> 'b))))

;; Assuming that current directory is the root of the repository.

(define (compile-port port) : CExpr
  (desugar
   (new-scope-phase
    (get-structured-python 
     (parse-python/port port (get-pypath))))))

(define (compile-file file)
  (compile-port  
   (open-input-file file)))

(define (compile-string str)
  (compile-port
   (open-input-string str)))

;; (define (get-globals-names (es : CExpr)) : (listof symbol)
;;   (type-case CExpr es
;;     (CLet (x type bind body)
;;           (cons x (get-globals-names body)))
;;     (else (list))))

; the built-in compile function
; compile: str * str * str
;          -- source * filename * mode
;; (define (compile args env sto)
;;   (check-types args env sto 'str 'str 'str
;;                (letrec ([source (MetaStr-s mval1)]
;;                         [filename (MetaStr-s mval2)]
;;                         [mode (MetaStr-s mval3)]
;;                         [code (compile-string source)]
;;                         [globals (list)])
;;                  (some (VObject '$code
;;                                 (some (MetaCode code filename globals))
;;                                 (hash empty))))))
