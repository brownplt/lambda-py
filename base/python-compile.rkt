#lang plai-typed/untyped

(require "python-core-syntax.rkt"
         "python-phases.rkt"
         "python-desugar.rkt"
         "util.rkt"
         (typed-in "parse-python.rkt" (parse-python/port : ('a 'b -> 'c)))
         (typed-in "parse-python.rkt" (parse-python/string : ('a 'b -> 'c)))         
         (typed-in "get-structured-python.rkt"
                   (get-structured-python : ('a -> 'b)))   
         (typed-in racket/base (open-input-file : ('a -> 'b)))
         (typed-in racket/base (open-input-string : ('a -> 'b))))      
         
(define (compile-port port)
  (desugar
   (new-scope-phase
    (get-structured-python
     (parse-python/port port (get-pypath))))))

(define (compile-string str)
  (compile-port
   (open-input-string str)))

;; (define (get-global-names (es : CExpr)) : (listof symbol)
;;   (type-case CExpr es
;;     (CModule (pre body)
;;              ;skip %locals trick
;;              (get-global-names (CSeq-e2 body)))
;;     (CLet (x type bind body)
;;           (cons x (get-global-names body)))
;;     (else (list))))

;; a more stable way to find globals in expr
(define (get-global-names expr-str)
  (get-module-level-globals
   (get-structured-python
    (parse-python/string expr-str (get-pypath)))))

;; built-in compile function, which takes
;; source, filename, mode and code class as its arguments
(define (compile args env sto)
  (check-types-pred args env sto MetaStr? MetaStr? MetaStr?
               (let* ([source (MetaStr-s mval1)] 
                      [filename (MetaStr-s mval2)]
                      [mode (MetaStr-s mval3)]
                      [code (compile-string source)]
                      [globals (get-global-names source)]
                      [code-class (fourth args)])
                 (some (VObjectClass 'code
                                     (some (MetaCode code filename globals))
                                     (hash empty)
                                     (some code-class))))))
