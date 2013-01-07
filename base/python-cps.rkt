#lang plai-typed

(require "python-core-syntax.rkt"
         (typed-in racket/base (append : ((listof 'a) (listof 'a) -> (listof'a)))))

(define (cps-genfunc [expr : CExpr]) : CExpr
  (type-case CExpr expr
    [CFunc (args sargs body method?)
           (CFunc args sargs
                  (make-seq
                   (list (make-seq (init-genfunc-args args))
                         ;; cps generator function body
                         (make-seq (make-genfunc-body args body))))
                  method?)]
    
    [else (CId 'TypeError (GlobalId))]))

(define (init-genfunc-args [args : (listof symbol)]) : (listof CExpr)
  (cons (CAssign (CId '^genobj-temp (LocalId)) (CUndefined))
        (map (lambda (e) (CAssign (CId e (LocalId)) (CId e (LocalId)))) args)))

(define (make-genfunc-body [args : (listof symbol)] [body : CExpr]) : (listof CExpr)
  (append
   (cons (CAssign (CId '^genobj-temp (LocalId)) 
                  (CApp (CId 'Generator (LocalId)) (list
                    ;; this is where we cps the actual gen func body
                    ;; by create a function to wrap it and the body is in cps form
                    (CFunc (list 'self) (none) 
                            (identity ;; TODO: should change to cps when complete
                             (make-seq
                              (list
                               (make-seq (map (lambda (e) (CAssign (CId e (LocalId)) (CUndefined))) args)) ;; assign arg to undefined
                               (CSeq (make-seq (map (lambda (e) (CAssign (CId e (LocalId)) (CGetField (CId 'self (LocalId)) e))) args))
                                     body))))
                            false))
                         (none)))
          (map (lambda (e) (CAssign (CGetField (CId '^genobj-temp (LocalId)) e) (CId e (LocalId)))) args))
   ;; return generator obj
   (list (CReturn (CId '^genobj-temp (LocalId))))))

(define (make-seq [exprs : (listof CExpr)]) : CExpr
  (foldl (lambda (e1 so-far) (CSeq so-far e1))
           (first exprs)
           (rest exprs)))

(define (cps [expr : CExpr]) : CExpr
  (type-case CExpr expr
    [CSeq (e1 e2) 
          (local ([define ^k (new-k)]) 
            (CFunc (list ^k) (none) 
                 (CReturn (CApp (cps e1) 
                                (list (CFunc (list '^_) (none) 
                                             (CReturn (CApp (cps e2) (list (CId ^k (LocalId))) (none)))
                                             false)) (none))) false))]
    ;; Suppose only one argument 
    [CApp (fun arges sarg)
             (local ([define ^k (new-k)]
                     [define ^fv (new-k)]
                     [define ^av (new-k)])
               (CFunc (list ^k) (none)
                      (CReturn (CApp (cps fun) 
                                     (list (CFunc (list ^fv) (none)
                                                  (CReturn (CApp (cps (first arges)) 
                                                                 (list (CFunc (list ^av) (none)
                                                                              (CReturn (CApp (CApp (CId ^fv (LocalId)) (list (CId ^av (LocalId))) (none)) (list (CId ^k (LocalId))) (none)))
                                                                              false)) (none))) false)) (none))) false))]
    [CFunc (args sargs body method?)
           (local ([define ^k (new-k)]
                   [define ^dyn-k (new-k)])
             (CFunc (list ^k) (none)
                    (CReturn (CApp (CId ^k (LocalId)) 
                                   (list (CFunc (list (first args)) (none)
                                                (CReturn (CFunc (list ^dyn-k) (none)
                                                                (CReturn (CApp 
                                                                          ;; save invokation continuation for early return
                                                                          (cps-let/cc '^RET body) (list (CId ^dyn-k (LocalId))) (none)))
                                                                false)) false)) (none))) false))]
   
    [CPrim2 (prim arg1 arg2)
            (local ([define ^k (new-k)]
                    [define ^arg1v (new-k)]
                    [define ^arg2v (new-k)])
              (CFunc (list ^k) (none)
                     (CReturn (CApp (cps arg1)
                                    (list (CFunc (list ^arg1v) (none)
                                                 (CReturn (CApp (cps arg2)
                                                                (list (CFunc (list ^arg2v) (none)
                                                                             (CReturn (CApp (CId ^k (LocalId)) 
                                                                                            (list (CPrim2 prim (CId ^arg1v (LocalId)) (CId ^arg2v (LocalId)))) (none)))
                                                                             false)) (none))) false)) (none))) false))]
    
    [CBuiltinPrim (op args)
                  (local ([define ^k (new-k)]
                          [define ^arg1v (new-k)]
                          [define ^arg2v (new-k)])
                    (CFunc (list ^k) (none)
                           (CReturn (CApp (cps (first args))
                                          (list (CFunc (list ^arg1v) (none)
                                                       (CReturn (CApp (cps (second args))
                                                                      (list (CFunc (list ^arg2v) (none)
                                                                                   (CReturn (CApp (CId ^k (LocalId))
                                                                                                  (list (CBuiltinPrim op (list (CId ^arg1v (LocalId)) (CId ^arg2v (LocalId))))) (none)))
                                                                                   false)) (none))) false)) (none))) false))]
    
    [CLet (x bind body)
          (cps (CApp (CFunc (list x) (none)
                            (CReturn body) false)
                     (list bind) (none)))]
         

    #|[CLet (x bind body)
          (local ([define ^k (new-k)]
                  [define ^bindv (new-k)])
            (CFunc (list ^k) (none)
                   (CReturn (CApp (cps bind)
                                  (list (CFunc (list ^bindv) (none)
                                               (CReturn (CLet x (CId ^bindv (LocalId))
                                                              (CApp (cps body) (list (CId ^k (LocalId))) (none)))) 
                                               false)) (none))) false))]|#
    
    
    [CId (x t) (cps-atomic expr)]
    [CUndefined () (cps-atomic expr)]
    [CList (es) (cps-atomic expr)]
    [CTuple (es) (cps-atomic expr)]
    [CStr (s) (cps-atomic expr)]
    
    [CAssign (t v) (local ([define ^k (new-k)]
                           [define ^v (new-k)])
                     (CFunc (list ^k) (none)
                            (CReturn (CApp (cps v)
                                           (list (CFunc (list ^v) (none) 
                                                        (CReturn (CApp (CId ^k (LocalId))
                                                                       (list (CAssign t (CId ^v (LocalId)))) (none))) false))
                                           (none))) false))]
    
    [CObject (c mval) (cps-atomic expr)]
    
    [CReturn (value)
             (cps (CApp (CId '^RET (LocalId)) (list value) (none)))]

    
    #|[CReturn (v) (local ([define ^k (new-k)]
                         [define ^v (new-k)])
                   (CFunc (list ^k) (none)
                        (CReturn (CApp (cps v)
                                       (list (CFunc (list ^v) (none)
                                                    (CReturn (CApp (CApp (CId '^RET (LocalId))
                                                                         (list (CId ^v (LocalId))) (none)) 
                                                                   (list (CId ^k (LocalId))) (none))) false)) (none))) false))]|#
    
    
    
    #|[CReturn (value)
             (CFunc (list '^k) (none)
                    (CReturn (CApp (cps value)
                                   (list (CFunc (list '^valuev) (none)
                                                (CReturn (CApp (CId '^k (LocalId)) (list (CId '^valuev (LocalId))) (none)))
                                                false)) (none))) false)]|#
   
 
          
    
    [else (error 'interp "haven't implemented a case yet")]))

;; get and increase unique k
(define new-k : (-> symbol)
  (local ([define n 0])
    (lambda ()
      (begin (set! n (add1 n))
             (string->symbol (string-append "^k-" (to-string n)))))))

;; atomic case
(define (cps-atomic [expr : CExpr]) : CExpr
  (local ([define k (new-k)])
    (CFunc (list k) (none) (CReturn (CApp (CId k (LocalId)) (list expr) (none))) false)))

(define (cps-let/cc [kont : symbol] [body : CExpr]) : CExpr
  (local ([define ^k (new-k)]
          [define ^v1 (new-k)]
          [define ^dyn-k (new-k)])
    (CFunc (list ^k) (none) 
           (CReturn (CLet kont (CFunc (list ^v1) (none)
                                      (CReturn (CFunc (list ^dyn-k) (none)
                                                     (CReturn (CApp (CId ^k (LocalId)) (list (CId ^v1 (LocalId))) (none))) false)) false)
                          (CApp (cps body) (list (CId ^k (LocalId))) (none)))) false)))
                                                     

#|(define (cps-let/cc [^kont : symbol] [body : CExpr]) : CExpr
  (CFunc (list '^k) (none) 
         (CReturn (CLet ^kont (CFunc (list '^v) (none)
                                     (CReturn (CApp (CId '^k (LocalId)) (list (CId '^v (LocalId))) (none))) false false)
                        (CApp body (list (CId '^k (LocalId))) (none)))) false false))|#

(define (c-identity) : CExpr
  (local ([define ^x (new-k)])
          (CFunc (list ^x) (none) (CReturn (CId ^x (LocalId))) false)))

(define (run-cps [e : CExpr])
  (CApp (cps e) (list (c-identity)) (none)))
