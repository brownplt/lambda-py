#lang racket

(require redex)
(provide (all-defined-out))


(define-language λπ
  ;; heap
  (Σ ((ref val) ...))
  ;; type of references
  (ref natural)
  
  ;; environment
  (ε ((x ref) ...))
  (εs (ε ...))
  
  ;; value types
  (val
   ;; NOTE(dbp): obj has optional metaval and optional class val, hence 4 variants
   (obj-val x mval ((string ref) ...))
   (obj-val x ((string ref) ...))
   (obj-val x mval ((string ref) ...) val)
   (obj-val x ((string ref) ...) val)
   ;; NOTE(dbp): closure, with/without vararg, with/without class name
   (fun-val εs (λ (x ...) e))
   (fun-val εs (λ (x ...) x e))
   (fun-val εs (λ (x ...) e) x)
   (fun-val εs (λ (x ...) x e) x)
   (pointer-val ref)
   undefined-val)
  
  ;; primitive operators
  (op string)
  
  ;; id-type
  (t global local)
  
  ;; types of meta-val
  (mval (meta-num number)
        (meta-str string)
        (meta-list (val ...))
        (meta-tuple (val ...))
        (meta-dict ((val val) ...))
        ;; set is like a list in representation, lack of order has to be accounted for in semantics
        (meta-set (val ...))
        (meta-class x)
        (meta-none)
        (meta-port)) ;; TODO(dbp): figure out how to represent port
  
  ;; types of expressions
  (e true
     false
     none
     (class x e e)
     (object x)
     (object x mval)
     (get-field e string)
     (seq e e)
     (assign e e)
     (if e e e)
     (id x t)
     (let (x t e) e)
     ;; NOTE(dbp): app with/without stararg
     (app e (e ...))
     (app e (e ...) e)
     ;; NOTE(dbp): 4 variants for presence/absence of stararg, symbol for class
     (fun (x ...) e)
     (fun (x ...) x e)
     (fun (x ...) e x)
     (fun (x ...) x e x)
     (while e e e)
     (loop e) ;; helper syntax for while
     (return e)
     (prim1 op e)
     (prim2 op e e)
     (builtin-prim op (e ...))
     (list e (e ...))
     (tuple e (e ...))
     (dict e ((e e) ...))
     (set e (e ...))
     ;; NOTE(dbp): empty raise is "reraise"
     (raise)
     (raise e)
     (tryexcept e x e e)
     (tryfinally e e)
     undefined
     break
     (module e e)
     r)
  
  ;; types for result
  (r val
     (return-r val)
     (exception-r val)
     break-r
     continue-r)
  
  ;; evaluation context
  (E hole
     (module E e)
     (assign e E)
     (seq E e)
     (if E e e)
     (let (x t E) e)
     (list E (e ...))
     (list val E)
     (tuple E (e ...))
     (tuple val E)
     (set E (e ...))
     (set val E)
     (dict E ((e e) ...))
     (dict val ((val val) ... (e E) (e e) ...))
     (dict val ((val val) ... (E val) (e e) ...)) ;; Python's dict has this evaluation order
     (get-field E string)
     (prim1 op E)
     (prim2 op E e)
     (prim2 op val E)
     (builtin-prim op E)
     (raise E)
     (return E)
     (tryexcept E x e e)
     (tryfinally E e)
     (loop E)
     (app E (e ...))
     (app val E)
     (app E (e ...) e)
     (app val E e)
     (app val (val ...) E)
     (val ... E e ...) ;; for list, tuple, app, etc.
     ;; todo - may need more
     )
  
  ;; context in a try block
  (T hole
     (assign e T)
     (seq T e)
     (if T e e)
     (let (x t T) e)
     (list T e)
     (list val T)
     (tuple T e)
     (tuple val T)
     (set T e)
     (set val T)
     (dict T e)
     (dict val ((val val) ... (e T) (e e) ...))
     (dict val ((val val) ... (T val) (e e) ...)) ;; Python's dict has this evaluation order
     (get-field T string)
     (prim1 op T)
     (prim2 op T e)
     (prim2 op val T)
     (builtin-prim op T)
     (raise T)
     (loop T)
     (app T (e ...))
     (app val T)
     (app T (e ...) e)
     (app val T e)
     (app val (val ...) T)
     (val ... T e ...) ;; for list, tuple, app, etc.
     ;; todo - may need more
     )
  
  ;; context for while body
  (H hole
     (assign e H)
     (seq H e)
     (if H e e)
     (let (x t H) e)
     (list H e)
     (list val H)
     (tuple H e)
     (tuple val H)
     (set H e)
     (set val H)
     (dict H e)
     (dict val ((val val) ... (e H) (e e) ...))
     (dict val ((val val) ... (H val) (e e) ...)) ;; Python's dict has this evaluation order
     (get-field H string)
     (prim1 op H)
     (prim2 op H e)
     (prim2 op val H)
     (builtin-prim op H)
     (raise H)
     (tryexcept H x e e)
     (tryfinally H e)
     (app H (e ...))
     (app val H)
     (app H (e ...) e)
     (app val H e)
     (app val (val ...) H)
     (val ... H e ...) ;; for list, tuple, app, etc.
     ;; todo - may need more
     )
  
  ;; identifiers, as per
  ;; http://docs.python.org/3.2/reference/lexical_analysis.html#keywords
  (x (variable-except False class finally is return
		      None continue for lambda try
		      True def from nonlocal while
		      and del global not with
		      as elif if or yield
		      assert else import pass
		      break except in raise))
  
  (p (e εs Σ))
  (P (E εs Σ))
  )
