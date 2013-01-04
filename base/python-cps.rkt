#lang plai-typed

(require "python-core-syntax.rkt"
         "python-syntax.rkt"
         "python-interp.rkt")


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
    
    [CReturn (value)
             (CFunc (list '^k) (none)
                    (CReturn (CApp (cps value)
                                   (list (CId '^k (GlobalId))) (none))) false false)] 
    
    [CObject (c mval)
             (CFunc (list '^k) (none)
                    (CReturn (CApp (CId '^k (GlobalId)) 
                                   (list expr) (none))) false false)]
    
    
    [CId (x t)
         (CFunc (list '^k) (none)
                    (CReturn (CApp (CId '^k (GlobalId)) 
                                   (list expr) (none))) false false)]
    
    [else (error 'interp "haven't implemented a case yet")]))



(define (run-cps [e : CExpr])
  (CApp (cps e) (list (CFunc (list '^x) (none) (CReturn (CId '^x (GlobalId))) false false)) (none)))


#|(interp (run-cps (CSeq (CPrim2 '+ (CObject 'num (some (MetaNum 3))) (CObject 'num (some (MetaNum 0))))
               (CPrim2 '- (CObject 'num (some (MetaNum 2))) (CObject 'num (some (MetaNum 0)))))))|#

;(run-cps (CObject 'num (some (MetaNum 3))))
(interp (run-cps (CObject 'num (some (MetaNum 5))))) 
(interp (run-cps (CSeq (CObject 'num (some (MetaNum 3)))
                       (CObject 'num (some (MetaNum 1210))))))
(interp (run-cps (CBuiltinPrim 'num+ (list (CObject 'num (some (MetaNum 3))) (CObject 'num (some (MetaNum 4)))))))
(interp (run-cps (CApp (CFunc (list 'x) (none) (CReturn 
                                                (CBuiltinPrim 'num+ (list (CId 'x (GlobalId)) (CObject 'num (some (MetaNum 4)))))) false false)
                       (list (CObject 'num (some (MetaNum 5)))) (none))))
;(interp (CApp (cps e) (
;(interp (CObject 'num (some (MetaNum 3))))