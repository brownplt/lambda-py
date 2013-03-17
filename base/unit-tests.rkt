#lang plai-typed/untyped

(require "util.rkt"
         "builtins/type.rkt"
         "python-interp.rkt"
         "python-syntax.rkt"
         "python-lexical-syntax.rkt"
         "python-core-syntax.rkt"
         "python-syntax-operations.rkt"
         (typed-in racket (format : (string 'a -> string))))


(test (list-subtract (list 'a 'b 'c 'd 'e) (list 'c 'e)) (list 'a 'b 'd))

(test/exn (chr "hi") "cannot convert hi into a single character")
(test/exn (chr "") "cannot convert  into a single character")

(test (contains-char? "hello" (chr "l") ) true)
(test (contains-char? "jasfdjoiewfa" (chr "-") ) false)
(test (contains-char? "custom-identifier" (chr "-")) true)
(test (contains-char? "customidentifier" (chr "-")) false)

(test (list-replace 2 63 (list 1 2 3 4)) (list 1 2 63 4))

(define ex1 (list (list 'o)))
(test (c3-merge ex1 empty) (some (list 'o)))
(define ex2 (list (list 'a 'o) 
                  (list 'o)))
(define ex3 (list (list 'a 'o)
                  (list 'b 'a 'o)
                  (list 'a 'b)))
(test (c3-merge ex3 empty) (none))
(define ex4 (list (list 'a 'o)
                  (list 'c 'a 'o)
                  (list 'b 'a 'o)
                  (list 'a 'c 'b)))
(test (c3-merge ex4 empty) (none))
(test (c3-merge ex2 empty) (some (list 'a 'o)))
(define ex5 (list (list 'd 'c 'b 'a 'o)
                  (list 'c 'a 'o)
                  (list 'b 'a 'o)
                  (list 'a 'o)
                  (list 'o)))
(test (c3-merge ex5 empty) (some (list 'd 'c 'b 'a 'o)))

(test (pyexpr-modify-tree
       (PyTuple (list (PyStr "ji") (PyTuple (list (PyStr "yo")))))
       (lambda (e)
         (type-case PyExpr e
           [PyStr (s) (PyLexId (string->symbol s) 'none)]
           [else (error 'pyexpr-modify-tree (format "pyexpr-modify-tree: Not a PyStr ~a" e))])))
      (LexTuple (list (PyLexId 'ji 'none) (LexTuple (list (PyLexId 'yo 'none)))))
      )

(test (Let 'x (CNone) (make-builtin-str "foo"))
      (CLet 'x (LocalId) (CNone)
        (make-builtin-str "foo")))

(test (Prim 'num< (make-builtin-str "foo") (make-builtin-str "bar"))
      (CBuiltinPrim 'num< (list (make-builtin-str "foo") (make-builtin-str "bar"))))

