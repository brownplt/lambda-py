#lang racket

(require redex)
(provide (all-defined-out))

(define-language λπ
  ;; heap
  (Σ ((ref v+undef) ...))
  ;; type of references
  (ref natural)
  
  ;; environment
  (ε ((x ref) ...))
  
  ;; value types
  ((v val)
	 (obj-val val mval ((string ref) ...))
	 (obj-val x mval ((string ref) ...))
   (pointer-val ref) (sym string))
  (v+undef v (undefined-val))
  (e+undef e (undefined-val))
  

  ;; primitive operators
  (op string)
  
  ;; id-type
  (t global local)
  
  ;; types of meta-val
  (mval (no-meta) (meta-num number) (meta-str string) (meta-none) ;; The Python value
        (meta-list (val ...)) (meta-tuple (val ...)) (meta-set (val ...))
        (meta-class x) (meta-code (x ...) x e)
        (meta-closure (λ (x ...) opt-var e)))

  (opt-var (x) (no-var))

  ;; types of expressions
  (e v ref (fetch e) (set! e e) (alloc e)
     (get-attr e e) (set-attr e e := e)
     (if e e e) (seq e e)
     (let (x t = e+undef) in e)
     (id x t) (assign e := e) (delete e)
     (app e (e ...)) (app e (e ...) e) (frame e) (return e)
     (while e e e) (loop e e) break continue
     (builtin-prim op (e ...))
     (fun (x ...) opt-var e)
     (object e mval) (list e (e ...))
     (tuple e (e ...)) (set e (e ...))
     (tryexcept e x e e) (tryfinally e e)
     (raise e) (err val)
     (module e e) (construct-module e)
     (in-module e ε))

  ;; evaluation context
  (E hole
     (fetch E) (set! E e) (set! val E)
     (alloc E)
     (module E e)
     (object E mval)
     (assign ref := E) (assign (id x global) := E)
     (set-attr E e := e) (set-attr v E := e) (set-attr v v := E)
     (seq E e)
     (if E e e)
     (let (x t = E) in e)
     (list E (e ...)) (list val Es)
     (tuple E (e ...)) (tuple val Es)
     (set E (e ...)) (set val Es)
     (get-attr E e) (get-attr val E)
     (builtin-prim op Es)
     (raise E)
     (return E)
     (tryexcept E x e e) (tryfinally E e)
     (loop E e) (frame E)
     (app E (e ...)) (app val Es)
     (app E (e ...) e) (app val Es e)
     (app val (val ...) E)
     (construct-module E) (in-module E ε))
  (Es (val ... E e ...))

  ;; context in a try block
  (T hole
     (fetch T) (set! T e) (set! val T)
     (alloc T)
     (object T mval)
     (assign ref := T) (assign (id x global) := T)
     (set-attr T e := e) (set-attr v T := e) (set-attr v v := T)
     (seq T e)
     (if T e e)
     (let (x t = T) in e)
     (list T e) (list val Ts)
     (tuple T e) (tuple val Ts)
     (set T e) (set val Ts)
     (get-attr T e) (get-attr val T)
     (builtin-prim op Ts)
     (raise T)
     (loop T e) (frame T)
     (app T (e ...)) (app val Ts)
     (app T (e ...) e) (app val Ts e)
     (app val (val ...) T)
     (construct-module T)
     )
  ;; Cases left out:
  ;; (tryfinally H e) ;; Don't go into try/finallys to find raises
  ;; (tryexcept T x e e) ;; Don't catch other try-blocks errors
  (Ts (val ... T e ...))

  ;; context for while body
  (H hole
     (fetch H) (set! H e) (set! val H)
     (alloc H)
     (object H mval)
     (assign ref := H) (assign (id x global) := H)
     (set-attr H e := e) (set-attr v H := e) (set-attr v v := H)
     (seq H e)
     (if H e e)
     (let (x t = H) in e)
     (list H e) (list val Hs)
     (tuple H e) (tuple val Hs)
     (set H e) (set val Hs)
     (get-attr H e) (get-attr val H)
     (builtin-prim op Hs)
     (raise H)
     (tryexcept H x e e) ;; DO go into try/catch to find break/continue
     (app H (e ...)) (app val Hs)
     (app H (e ...) e) (app val Hs e)
     (app val (val ...) H)
     (construct-module E)
     )
  ;; Cases left out:
  ;; (tryfinally H e) ;; Don't go into try/finallys to find break/continue
  ;; (loop H e) ;;  Don't go into other loops to find break/continue
  ;; (frame H) ;; Don't go into nested function calls to find breaks
  (Hs (val ... H e ...))

  ;; contexts for returning
  (R hole
     (fetch R) (set! R e) (set! val R)
     (alloc R)
     (object R mval)
     (assign ref := R) (assign (id x global) := R)
     (set-attr R e := e) (set-attr v R := e) (set-attr v v := R)
     (seq R e)
     (if R e e)
     (let (x t = R) in e)
     (list R e) (list val Rs)
     (tuple R e) (tuple val Rs)
     (set R e) (set val Rs)
     (get-attr R e) (get-attr val R)
     (builtin-prim op Rs)
     (raise R)
     (loop R e) ;; DO go into active loops to find returns
     (tryexcept R x e e) ;; DO go into try/catches to find returns
     (app R (e ...)) (app val Rs)
     (app R (e ...) e) (app val Rs e)
     (app val (val ...) R)
     (construct-module E)
     )
  ;; Cases left out:
  ;; (frame R) ;; Don't go into other functions to find returns
  ;; (tryfinally R e) DON'T go into try/finallys to find returns
  (Rs (val ... R e ...))

  ;; identifiers
  ((w x y z f g h) (variable-except λ δ no-meta no-var))
  
  (p (e ε Σ))
  (P (E ε Σ))
  )
