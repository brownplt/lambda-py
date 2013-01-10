#lang plai-typed

(require "python-core-syntax.rkt"
         "python-desugar.rkt"
         "util.rkt"
         "builtins/str.rkt"
         (typed-in "get-structured-python.rkt"
                   (get-structured-python : ('a -> 'b)))
         (typed-in "parse-python.rkt"
                   (parse-python/port : ('a string -> 'b)))
         (typed-in racket/base (open-input-file : ('a -> 'b)))
         (typed-in racket/base (open-input-string : ('a -> 'b))))

;; Assuming that current directory is the root of the repository.

(define (compile-file file)
  (compile-port  
   (open-input-file file)))

(define (compile-string str)
  (compile-port
   (open-input-string str)))

(define (compile-port port)
  (desugar 
   (get-structured-python 
    (parse-python/port 
     port
     (get-pypath)))))

; the built-in compile function
(define (compile args env sto)
  (check-types args env sto 'str 'str 'str
               (letrec ([source (MetaStr-s mval1)]
                        [filename (MetaStr-s mval2)]
                        [mode (MetaStr-s mval3)]
                        [code (compile-string source)]
                        [body (CModule-body code)]
                        [names (CModule-names code)]
                        ; Replacing prelude with None before wrapping it in python-lib to avoid 
                        ; initializing the names to Undefined. 
                        ; When used with exec, it is expected that the names are already initialized
                        ; in the globals.
                        [code2 (python-lib (CModule names (CNone) body))])
                 (some (VObject '$code
                                (some (MetaCode code2 filename names))
                                (make-hash empty))))))
