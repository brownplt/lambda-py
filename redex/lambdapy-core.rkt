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
   ;; object has class and body-val - not sure how to do meta-vals yet
   (obj-val string ((string ref) ...))
   (fun-val (λ (x ...) e)))

  (id-type global nonlocal local)

  (meta-val
   (meta-num number)
   (meta-str string)
   (meta-list ))
  
  ;; types of expressions
  (e
   string
   true
   false
   none
   (class string string e)
   ;; how to do metavals?
   (object string ??)
   (get-field e string)
   (seq e e)
   (assign e e)
   (error e)
   (if e e e)
   (id string id-type)
   (let (x e) e)
   ;; we have two variants of app, with and without stararg
   (app e (e ...))
   (app-star e (e ...) e)
   ;; method?
   (fun (x ...) e)
   (fun-var (x ...) string e)
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
   (except-name (e ...) string e)
   undefined
   break
   (module e e))

  ;; types for evaluation.
  (E
   hole
   ;; todo
   )
  )
