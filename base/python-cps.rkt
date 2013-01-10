#lang plai-typed

(require "python-core-syntax.rkt")

(define (cps-genfunc [expr : CExpr]) : CExpr
  (type-case CExpr expr
    [CFunc (args sargs body method?)
           ;; TODO: cps generator function body
           ;; temperary cps body for now
           (cps body)]
    [else (CStr "none")]
           ))

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
                                                                              (CReturn (CApp (CApp (CId ^fv (LocalId)) 
                                                                                                   (list (CId ^av (LocalId))) (none)) (list (CId ^k (LocalId))) (none)))
                                                                              false)) (none))) false)) (none))) false))]
    
                                     
 
    ;; Temporarily for generator function
    [CFunc (args sargs body method?)
           (local ([define ^k (new-k)]
                   [define ^dyn-k (new-k)]
                   [define ^k2 (new-k)])
             (CFunc (list ^k) (none)
                    (CReturn (CApp (CId ^k (LocalId)) 
                                   (list
                                    ;(CSeq (CAssign (CId '^RET (LocalId)) (CNone))
                                    (CLet '^RET (CNone)
                                          (CLet '^RESUMER (CNone)
                                                (CSeq (CAssign (CId '^RESUMER (LocalId))
                                                               (CFunc (list '^v) (none)
                                                                      (CReturn (CApp (cps body)
                                                                                     (list (CFunc (list ^k2) (none)
                                                                                                  (CReturn (CError (CObject 'str (some (MetaStr "StopIteration"))))) false)) (none))) false))
                                                      (CFunc (list '^a) (none)
                                                             (CReturn (cps-let/cc ^dyn-k
                                                                                  (CSeq
                                                                                   (CAssign (CId '^RET (NonlocalId)) (CId ^dyn-k (LocalId))) 
                                                                                   ;(cps body)
                                                                                   (CApp (CId '^RESUMER (LocalId)) (list (CId '^a (LocalId))) (none))
                                                                                   ))) false))))) (none))) false))]
    
    ;; Original function
    #|[CFunc (args sargs body method?)
           (local ([define ^k (new-k)]
                   [define ^dyn-k (new-k)])
             (CFunc (list ^k) (none)
                    (CReturn (CApp (CId ^k (LocalId)) 
                                   (list (CFunc (list (first args)) (none)
                                                (CReturn (cps-let/cc '^RET (cps body))) false)) (none))) false))]
    |#
    
    [CReturn (value)
             (cps (CApp (CId '^RET (LocalId)) (list value) (none)))]
    
    
    [CYield (v) (local ([define ^gen-k (new-k)]
                        [define ^v (new-k)])
                  (CFunc (list ^gen-k) (none)
                         (CReturn (CApp (cps v)
                                        (list (CFunc (list ^v) (none)
                                                     (CReturn (CSeq
                                                               (CAssign (CId '^RESUMER (NonlocalId)) (CId ^gen-k (LocalId)))
                                                               (CApp (CId '^RET (LocalId))
                                                                    (list (CId ^v (LocalId))) (none)))) 
                                                     false)) (none))) false))]
  
    #|[CReturn (v) (local ([define ^gen-k (new-k)]
                         [define ^v (new-k)])
                   (CFunc (list ^gen-k) (none)
                          (CReturn (CApp (cps v)
                                         (list (CFunc (list ^v) (none)
                                                      (CReturn (CApp (CId '^RET (LocalId))
                                                                     (list (CId ^v (LocalId))) (none))) 
                                                      false)) (none))) false))]|#
    

   
    #|[CReturn (v) (local ([define ^k (new-k)]
                        [define ^v (new-k)])
                  (CFunc (list ^k) (none)
                         (CReturn (CApp (cps v)
                                        (list (CFunc (list ^v) (none)
                                                     (CReturn (CApp (CApp (CId '^RET (LocalId))
                                                                          (list (CId ^v (LocalId))) (none)) 
                                                                    (list (CId ^k (LocalId))) (none))) false)) (none))) false))]
    |#
   
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
          (cps-let x (cps bind) (cps body))]
     
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

(define (cps-let/cc [kont : symbol] [body/k : CExpr]) : CExpr
  (local ([define ^k (new-k)]
          [define ^v1 (new-k)]
          [define ^dyn-k (new-k)])
    (CFunc (list ^k) (none) 
           (CReturn (CLet kont (CFunc (list ^v1) (none)
                                      (CReturn (CFunc (list ^dyn-k) (none)
                                                     (CReturn (CApp (CId ^k (LocalId)) (list (CId ^v1 (LocalId))) (none))) false)) false)
                          (CApp body/k (list (CId ^k (LocalId))) (none)))) false)))
                                                     

(define (cps-let [x : symbol] [bind/k : CExpr] [body/k : CExpr])
  (local ([define ^k (new-k)])
            (CFunc (list ^k) (none)
                   (CReturn (CApp bind/k
                                  (list (CFunc (list x) (none)
                                               (CReturn (CApp body/k (list (CId ^k (LocalId))) (none))) false)) (none))) false)))

(define (c-identity) : CExpr
  (local ([define ^x (new-k)])
          (CFunc (list ^x) (none) (CReturn (CId ^x (LocalId))) false)))

(define (run-cps [e : CExpr])
  (CApp (cps e) (list (c-identity)) (none)))
