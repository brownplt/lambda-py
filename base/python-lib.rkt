#lang plai-typed

(require "python-core-syntax.rkt")
(require "builtins/num.rkt"
         "builtins/str.rkt"
         "builtins/list.rkt"
         "builtins/tuple.rkt"
         "builtins/dict.rkt"
         "builtins/object.rkt"
         "builtins/bool.rkt"
         "builtins/set.rkt"
         "builtins/none.rkt"
         "builtins/file.rkt"
         "builtins/type.rkt"
         "builtins/method.rkt"
         "util.rkt"
         "python-lib-bindings.rkt"
         (typed-in "get-structured-python.rkt"
                   (get-structured-python : ('a -> 'b)))
         (typed-in "parse-python.rkt"
                   (parse-python/port : ('a string -> 'b)))
         (typed-in racket/base (open-input-file : ('a -> 'b)))
         "python-syntax.rkt"
         "python-lexical-syntax.rkt"
         "python-desugar.rkt"
         "python-phase1.rkt"
         (typed-in racket/base (append : ((listof 'a) (listof 'a) (listof 'a) (listof 'a) (listof 'a) -> (listof 'a)))))

#|

Here is a suggestion for how to implement shared runtime functionality -
write it as core expression forms and use python-lib to wrap your
desugared expressions in an environment that will contain useful
bindings.  For example, this sample library binds `print` to a function
that calls the primitive `print`.

|#

;; these are builtin functions that we have written in actual python files which
;; are pulled in here and desugared for lib purposes
(define (get-pylib-programs)
  (map (lambda(file) 
         (desugar 
           (new-scope-phase
             (get-structured-python 
               (parse-python/port 
                 (open-input-file file)
                 (get-pypath))))))
       (list "pylib/range.py"
             "pylib/seq_iter.py"
             "pylib/filter.py"
             "pylib/any.py"
             "pylib/all.py"
             "pylib/dicteq.py"
             "py-prelude.py"
            )))
             

(define-type-alias Lib (CExpr -> CExpr))

(define (python-lib [expr : CExpr]) : CExpr
  (local [(define (cascade-lets bindings body)
            (if (empty? bindings)
                body
                (local [(define b (first bindings))]
                  (CLet (bind-left b) (GlobalId) (bind-right b)
                      (cascade-lets (rest bindings) body)))))]
    (cascade-lets lib-function-dummies
                  (seq-ops (append
                             (map (lambda (b) (bind-right b)) lib-functions)
                             (get-pylib-programs)
                             (list (CModule-body expr))
                             empty empty)))))
