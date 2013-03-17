#lang plai-typed/untyped

(require "python-core-syntax.rkt"
         "python-phases.rkt"
         "python-desugar.rkt"
         "util.rkt"
         (typed-in "parse-python.rkt" (parse-python/port : ('a 'b -> 'c)))
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

(define (get-global-names (es : CExpr)) : (listof symbol)
  (type-case CExpr es
    (CModule (pre body) (get-global-names body))
    (CLet (x type bind body)
          (cons x (get-global-names body)))
    (else (list))))

;; built-in compile function, which takes
;; source, filename, mode as its argument
(define (compile args env sto)
  (check-types-pred args env sto MetaStr? MetaStr? MetaStr?
               (let* ([source (MetaStr-s mval1)] 
                      [filename (MetaStr-s mval2)]
                      [mode (MetaStr-s mval3)]
                      [code (compile-string source)]
                      [globals (get-global-names code)])
                 (some (VObject 'code
                                (some (MetaCode code filename globals))
                                (hash empty))))))
