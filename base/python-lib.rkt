#lang plai-typed/untyped

(require "python-core-syntax.rkt")
(require "builtins/num.rkt"
         "builtins/str.rkt"
         "builtins/list.rkt"
         "builtins/tuple.rkt"
         "builtins/dict.rkt"
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
         (typed-in "parse-python.rkt"
                   (parse-python/port : ('a string -> 'b)))
         (typed-in racket/base (open-input-file : ('a -> 'b)))
         (typed-in racket/base (close-input-port : ('a -> 'b)))
         "python-syntax.rkt"
         "python-macros.rkt"
         "python-lexical-syntax.rkt"
         "python-desugar.rkt"
         "python-phases.rkt"
         (typed-in racket/base (append : ((listof 'a) (listof 'a) (listof 'a) (listof 'a) (listof 'a) -> (listof 'a)))))

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#

(define pylib-programs (none))
;; these are builtin functions that we have written in actual python files which
;; are pulled in here and desugared for lib purposes
(define (get-pylib-programs paths)
  (type-case (optionof (listof CExpr)) pylib-programs
    [none ()
     (begin
       (set! pylib-programs
        (some
          (map (lambda (file) 
            (local [
              (define f (open-input-file file))
              (define pyast (parse-python/port f (get-pypath)))
            ]
            (begin
              (close-input-port f)
              (desugar 
                (desugar-macros
                  (new-scope-phase
                    (get-structured-python pyast)))))))
               paths)))
         (some-v pylib-programs))]
     [some (v) v]))
                 

(define-type-alias Lib (CExpr -> CExpr))
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
