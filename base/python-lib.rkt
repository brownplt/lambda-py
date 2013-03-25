#lang plai-typed/untyped

(require "python-core-syntax.rkt")
(require "builtins/num.rkt"
         "builtins/str.rkt"
         "builtins/list.rkt"
         "builtins/tuple.rkt"
         "builtins/object.rkt"
         "builtins/bool.rkt"
         "builtins/set.rkt"
         "builtins/file.rkt"
         "builtins/method.rkt"
         "modules/builtin-modules.rkt"
         "util.rkt"
         "python-lib-bindings.rkt"
         (typed-in "python-lib-list.rkt" (python-libs : (listof string)))
         (typed-in "get-structured-python.rkt"
                   (get-structured-python : ('a -> 'b)))
         (typed-in "parse-python.rkt" (parse-python/port : ('a string -> 'b)))
         "core-to-sexp.rkt"
         (typed-in "sexp-to-core.rkt" (sexp->core : ('a -> CExpr)))
         (typed-in racket/fasl (s-exp->fasl : ('a 'b -> 'c)) (fasl->s-exp : ('a -> 'b)))
         "python-syntax.rkt"
         "python-macros.rkt"
         "python-lexical-syntax.rkt"
         "python-desugar.rkt"
         "python-phases.rkt"
         (typed-in racket/string
          (string-split : (string string -> (listof string))))
         (typed-in racket/base
          (open-input-file : ('a -> 'b))
          (open-output-file : ('a -> 'b))
          (close-input-port : ('a -> 'b))
          (close-output-port : ('a -> 'b))
          (path->string : ('a -> string))
          (file-exists? : ('a -> boolean))
          (delete-file : ('a -> 'b))
          (file-or-directory-modify-seconds : (string (-> number) -> number))
          (append : ((listof 'a) (listof 'a) (listof 'a) (listof 'a) (listof 'a) -> (listof 'a)))))

(define CACHE_EXT ".pyc") ;; thatsthejoke.jpg
(define (get-ast/cache path)
  (local
    [(define without-ext (first (string-split (path->string path) ".")))
     (define with-ext (string-append without-ext CACHE_EXT))
     (define time-python-file-modified (file-or-directory-modify-seconds path #f))
     (define time-cache-was-created (file-or-directory-modify-seconds with-ext #f (lambda () 0)))]
  (cond
    [(>= time-python-file-modified time-cache-was-created)
     (begin
       (when (file-exists? with-ext) (delete-file with-ext))
       (local
        [(define f (open-input-file path))
         (define pyast (parse-python/port f (get-pypath)))
         (define core-ast
                 (desugar 
                  (desugar-macros
                    (new-scope-phase
                      (get-structured-python pyast)))))
         (define cache-file (open-output-file with-ext))]
         (begin
          (s-exp->fasl (core->sexp core-ast) cache-file)
          (close-output-port cache-file)
          (close-input-port f)
          core-ast)))]
    [else
     (local
      [(define cache-file (open-input-file with-ext))
       (define core-ast (sexp->core (fasl->s-exp cache-file)))]
      (begin
        (close-input-port cache-file)
        core-ast))])))
     
  

(define pylib-programs (none))
;; these are builtin functions that we have written in actual python files which
;; are pulled in here and desugared for lib purposes
(define (get-pylib-programs paths)
  (type-case (optionof (listof CExpr)) pylib-programs
    [none ()
     (begin
       (set! pylib-programs
        (some
          (map get-ast/cache paths)))
         (some-v pylib-programs))]
    [some (v) v]))
                 

(define-type-alias Lib (CExpr -> CExpr))

;; built-in functions and classes
(define builtins-symbol
  (map (lambda (lib)
         (bind-left lib))
       lib-function-dummies))


(define (cascade-lets bindings body)
            (if (empty? bindings)
                body
                (local [(define b (first bindings))]
                  (CLet (bind-left b) (GlobalId) (bind-right b)
                      (cascade-lets (rest bindings) body)))))
(define (python-lib [expr : CExpr]) : CExpr
    (cascade-lets lib-function-dummies
                  (seq-ops (append
                             (map (lambda (b) (bind-right b)) lib-functions)
                             (get-pylib-programs python-libs)
                             (map (lambda (b) (bind-right b))
                                  (list (bind 'True (assign 'True (CTrue)))
                                        (bind 'False (assign 'False (CFalse)))))
                             (get-builtin-modules)
                             (list (CModule-body expr))))))
