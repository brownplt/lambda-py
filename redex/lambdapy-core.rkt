#lang racket

(require redex)
(provide (all-defined-out))


(define-language λπ
  ;; heap
  (Σ ((ref val) ...))
  ;; type of references
  (ref natural)
  
  ;; value types
  (val
   ;; core string type, not string class
   (str-val string)
   ;; built in bool types
   (true-val)
   (false-val)
   ;; none type
   (none-val)
   ;; list and tuple types, just reference heap values
   (list-val (ref ...))
   (tuple-val (ref ...))
   ;; dict has key/value pairs (all on heap)
   (dict-val ((ref ref) ...))
   ;; set is like a list in representation, lack of order has
   ;; to be accounted for in semantics
   (set-val (ref ...))
   ;; class has name, base, and body
   (class-val string string e)
   ;; object has class and body-val, with/without meta-val - not sure how to do meta-vals yet
   (obj-val x meta-val ((string ref) ...))
   (obj-val x ((string ref) ...))
   ;; closure
   (fun-val (λ (x ...) e))
   ;; undefined
   (undefined-val))
  
  ;; id-type
  (t global nonlocal local)

  (meta-val
   (meta-num number)
   (meta-str string)
   (meta-list (val ...))
   (meta-tuple (val ...))
   (meta-dict ((val val) ...))
   (meta-set (val ...))
   (meta-class string)
   (meta-none))
  
  ;; types of expressions
  (e
   (str string)
   true
   false
   none
   (class x x e)
   ;; with/without meta-val - how to do metavals?
   (object x)
   (object x meta-val)
   (get-field e string)
   (seq e e)
   (assign e e)
   (error e)
   (if e e e)
   (id x t)
   (let (x e) e)
   ;; we have two variants of app, with and without stararg
   (app e (e ...))
   (app-star e (e ...) e)
   (fun (x ...) e)
   (fun (x ...) x e)
   ;; put method as a separate construct for now
   (method (x ...) e)
   (method (x ...) x e)
   (while e e e)
   (return e)
   (prim1 string e)
   (prim2 string e e)
   (builtin-prim string (e ...))
   (list (e ...))
   (tuple (e ...))
   (dict ((e e) ...))
   (set (e ...))
   (reraise)
   (raise e)
   (try e (e ...) e e)
   (except (e ...) e)
   (except (e ...) x e)
   undefined
   break
   (module e e)
   val)

  ;; types for evaluation.
  (E
   hole
   (module E e)
   (module val E)
   (assign e E)
   (assign E val)
   (seq E e)
   (if E e e)
   (while E e e)
   (let (x E) e)
   (app E (e ...))
   (app val (val ... E e ...))
   (app-star E (e ...) e)
   (app-star val (val ... E e ...) e)
   (app-star val (val ...) E)
   ;; todo - may need more
   )
  
  ;; identifiers, more keywords will be filled in
  (x (variable-except if while except lambda))
  
  (p (e Σ))
  (P (E Σ))
  )
