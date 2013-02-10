#lang plai-typed 

(require "../python-core-syntax.rkt"
         "../util.rkt"
         "num.rkt"
         "str.rkt"
         "object.rkt")

(define bool-class 
  (seq-ops (list
             (CAssign (CId 'bool (GlobalId))
                      (CClass 
                        'bool
                        (list 'int)
                        (CNone)))
             
             (def 'bool '__init__
                  (CFunc (list 'self) (some 'args)
                         (CAssign (CId 'self (LocalId))
                                  (CBuiltinPrim 'bool-init
                                                (list
                                                 (CId 'args (LocalId)))))
                         (some 'bool)))

             (def 'bool '__str__
                  (CFunc (list 'self) (none)
                         (CIf (CApp (CGetField (CId 'self (LocalId)) '__eq__)
                                    (list (make-builtin-num 1))
                                    (none))
                              (CReturn (make-builtin-str "True"))
                              (CReturn (make-builtin-str "False")))
                         (some 'bool)))
             
             (def 'bool '__int__
                  (CFunc (list 'self) (none)
                         (CReturn (CApp (CGetField (CId 'self (LocalId)) '__add__) 
                                        (list (make-builtin-num 0))
                                        (none)))
                         (some 'bool)))

             (def 'bool '__float__
                  (CFunc (list 'self) (none)
                         (CReturn (CApp (CGetField (CId 'self (LocalId)) '__add__) 
                                        (list (make-builtin-num 0.0))
                                        (none)))
                         (some 'bool))))))

(define (make-builtin-bool [b : boolean]) : CExpr
  (CObject 
    'bool
    (some 
      (if b 
        (MetaNum 1)
        (MetaNum 0)))))

(define (bool-init [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (local [(define meta-startuple (MetaTuple-v (some-v (VObjectClass-mval (first args)))))]
     (if (= (length meta-startuple) 0)
       (some false-val) 
       (type-case CVal (first meta-startuple) 
                  [VClosure (e a s b c) (some true-val)] 
                  [VObjectClass (a mval d class)
                                (if (truthy-object? (VObject a mval d)) 
                                    (some true-val) 
                                    (some false-val))]
                  [VUndefined () (some false-val)]
                  [else (error 'bool-init "Should not initialize boolean from pointer")]))))
