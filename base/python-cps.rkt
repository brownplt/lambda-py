#lang plai-typed

(require "python-core-syntax.rkt")


(define (cps [expr : CExpr])
  (type-case CExpr expr
    [CSeq (e1 e2) 
          (CFunc (list '^k) (none) 
                 (CReturn (CApp (cps e1) 
                                (list (CFunc (list '^_) (none) 
                                             (CReturn (CApp (cps e2) (list (CId '^k (LocalId))) (none)))
                                             false)) (none))) false)]
    ;; Suppose only one argument 
    [CApp (fun arges sarg)
          (CFunc (list '^k) (none)
                 (CReturn (CApp (cps fun) 
                                (list (CFunc (list '^fv) (none)
                                             (CReturn (CApp (cps (first arges)) 
                                                            (list (CFunc (list '^av) (none)
                                                                         (CReturn (CApp (CApp (CId '^fv (LocalId)) (list (CId '^av (LocalId))) (none)) (list (CId '^k (LocalId))) (none)))
                                                                         false)) (none))) false)) (none))) false)]
    [CFunc (args sargs body method?)
           (CFunc (list '^k) (none)
                  (CReturn (CApp (CId '^k (LocalId)) 
                                 (list (CFunc (list (first args)) (none)
                                              (CReturn (CFunc (list '^dyn-k) (none)
                                                              (CReturn (CApp (cps body) (list (CId '^dyn-k (LocalId))) (none)))
                                                              false)) false)) (none))) false)]
    
    [CPrim2 (prim arg1 arg2)
            (CFunc (list '^k) (none)
                   (CReturn (CApp (cps arg1)
                                  (list (CFunc (list '^arg1v) (none)
                                               (CReturn (CApp (cps arg2)
                                                              (list (CFunc (list '^arg2v) (none)
                                                                           (CReturn (CApp (CId '^k (LocalId)) 
                                                                                          (list (CPrim2 prim (CId '^arg1v (LocalId)) (CId '^arg2v (LocalId)))) (none)))
                                                                           false)) (none))) false)) (none))) false)]
    
    [CBuiltinPrim (op args)
                  (CFunc (list '^k) (none)
                         (CReturn (CApp (cps (first args))
                                        (list (CFunc (list '^arg1v) (none)
                                                     (CReturn (CApp (cps (second args))
                                                                    (list (CFunc (list '^arg2v) (none)
                                                                                 (CReturn (CApp (CId '^k (LocalId))
                                                                                                (list (CBuiltinPrim op (list (CId '^arg1v (LocalId)) (CId '^arg2v (LocalId))))) (none)))
                                                                                 false)) (none))) false)) (none))) false)]
    
    [CId (x t) (cps-atomic expr)]
    [CList (es) (cps-atomic expr)]
    [CTuple (es) (cps-atomic expr)]
    [CStr (s) (cps-atomic expr)]
    [CAssign (t v) (CFunc (list '^k) (none)
                          (CReturn (CApp (cps v)
                                         (list (CFunc (list '^v) (none) 
                                                      (CReturn (CApp (CId '^k (LocalId))
                                                                     (list (CAssign t (CId '^v (LocalId)))) (none))) false))
                                                                     (none))) false)]
    [CObject (c mval) (cps-atomic expr)]
    
    [CReturn (value)
             (CFunc (list '^k) (none)
                    (CReturn (CApp (cps value)
                                   (list (CId '^k (LocalId))) (none))) false)]
    
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

(define (c-identity) : CExpr
  (CFunc (list '^x) (none) (CReturn (CId '^x (LocalId))) false))

(define (run-cps [e : CExpr])
  (CApp (cps e) (list (c-identity)) (none)))
