#lang plai-typed

(require "python-core-syntax.rkt")


(define (cps [expr : CExpr])
  (type-case CExpr expr
    [CSeq (e1 e2) 
          (CFunc (list '^k) (none) 
                 (CReturn (CApp (cps e1) 
                                (list (CFunc (list '^_) (none) 
                                             (CReturn (CApp (cps e2) (list (CId '^k (GlobalId))) (none)))
                                             false false)) (none))) false false)]
    ;; Suppose only one argument 
    [CApp (fun arges sarg)
          (CFunc (list '^k) (none)
                 (CReturn (CApp (cps fun) 
                                (list (CFunc (list '^fv) (none)
                                             (CReturn (CApp (cps (first arges)) 
                                                            (list (CFunc (list '^av) (none)
                                                                         (CReturn (CApp (CApp (CId '^fv (GlobalId)) (list (CId '^av (GlobalId))) (none)) (list (CId '^k (GlobalId))) (none)))
                                                                         false false)) (none))) false false)) (none))) false false)]
    [CFunc (args sargs body method? yield?)
           (CFunc (list '^k) (none)
                  (CReturn (CApp (CId '^k (GlobalId)) 
                                 (list (CFunc (list (first args)) (none)
                                              (CReturn (CFunc (list '^dyn-k) (none)
                                                              (CReturn (CApp (cps body) (list (CId '^dyn-k (GlobalId))) (none)))
                                                              false false)) false false)) (none))) false false)]
    
    [CPrim2 (prim arg1 arg2)
            (CFunc (list '^k) (none)
                   (CReturn (CApp (cps arg1)
                                  (list (CFunc (list '^arg1v) (none)
                                               (CReturn (CApp (cps arg2)
                                                              (list (CFunc (list '^arg2v) (none)
                                                                           (CReturn (CApp (CId '^k (GlobalId)) 
                                                                                          (list (CPrim2 prim (CId '^arg1v (GlobalId)) (CId '^arg2v (GlobalId)))) (none)))
                                                                           false false)) (none))) false false)) (none))) false false)]
    
    [CBuiltinPrim (op args)
                  (CFunc (list '^k) (none)
                         (CReturn (CApp (cps (first args))
                                        (list (CFunc (list '^arg1v) (none)
                                                     (CReturn (CApp (cps (second args))
                                                                    (list (CFunc (list '^arg2v) (none)
                                                                                 (CReturn (CApp (CId '^k (GlobalId))
                                                                                                (list (CBuiltinPrim op (list (CId '^arg1v (GlobalId)) (CId '^arg2v (GlobalId))))) (none)))
                                                                                 false false)) (none))) false false)) (none))) false false)]
    
    [CId (x t) (cps-atomic expr)]
    [CList (es) (cps-atomic expr)]
    [CTuple (es) (cps-atomic expr)]
    [CStr (s) (cps-atomic expr)]
    [CAssign (t v) (CFunc (list '^k) (none)
                          (CReturn (CApp (cps v)
                                         (list (CFunc (list '^v) (none) 
                                                      (CReturn (CApp (CId '^k (GlobalId))
                                                                     (list (CAssign t (CId '^v (GlobalId)))) (none))) false false))
                                                                     (none))) false false)]
    [CObject (c mval) (cps-atomic expr)]
    
    [CReturn (value)
             (CFunc (list '^k) (none)
                    (CReturn (CApp (cps value)
                                   (list (CId '^k (GlobalId))) (none))) false false)]
    
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
    (CFunc (list k) (none) (CReturn (CApp (CId k (GlobalId)) (list expr) (none))) false false)))

(define (identity) : CExpr
  (local ([define k (new-k)])
    (CFunc (list k) (none) (CReturn (CId k (GlobalId))) false false)))

(define (run-cps [e : CExpr])
  (CApp (cps e) (list (identity)) (none)))
