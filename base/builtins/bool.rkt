#lang plai-typed 

(require "../python-core-syntax.rkt"
         "../util.rkt"
         "num.rkt"
         "str.rkt"
         "object.rkt")

(define bool-class 
  (CClass 
    'bool
    'int
    (seq-ops (list
               (def '__init__
                    (CFunc (list 'self) (some 'args)
                           (CReturn (CBuiltinPrim 'bool-init
                                                  (list
                                                   (CId 'args (LocalId)))))
                           true))

               (def '__str__
                    (CFunc (list 'self) (none)
                           (CIf (CApp (CGetField (CId 'self (LocalId)) '__eq__)
                                      (list (CId 'self (LocalId)) (make-builtin-num 1))
                                      (none))
                                (CReturn (make-builtin-str "True"))
                                (CReturn (make-builtin-str "False")))
                           true))
               (def '__int__
                    (CFunc (list 'self) (none)
                           (CReturn (CApp (CGetField (CId 'self (LocalId)) '__add__) 
                                          (list (CId 'self (LocalId)) 
                                                (make-builtin-num 0))
                                          (none)))
                           true))

               (def '__float__
                    (CFunc (list 'self) (none)
                           (CReturn (CApp (CGetField (CId 'self (LocalId)) '__add__) 
                                          (list (CId 'self (LocalId)) 
                                                (make-builtin-num 0.0))
                                          (none)))
                           true))))))

(define (make-builtin-bool [b : boolean]) : CExpr
  (CObject 
    'bool
    (some 
      (if b 
        (MetaNum 1)
        (MetaNum 0)))))

(define (bool-init [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (local [(define meta-startuple (MetaTuple-v (some-v (VObject-mval (first args)))))]
     (if (= (length meta-startuple) 0)
       (some false-val) 
       (type-case CVal (first meta-startuple) 
                  [VClosure (e a s b) (some true-val)] 
                  [VObject (a mval d) (if (truthy-object? (VObject a mval d)) 
                                        (some true-val) 
                                        (some false-val))]
                  [VUndefined () (some false-val)]))))
