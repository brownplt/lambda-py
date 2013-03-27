#lang plai-typed/untyped

(require "../python-core-syntax.rkt"
         "../util.rkt"
         "num.rkt"
         "str.rkt"
         "object.rkt")

(define (make-builtin-bool [b : boolean]) : CExpr
  (if b
      (CTrue)
      (CFalse)))

(define (bool-init [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (local [(define meta-startuple (MetaTuple-v (some-v (VObjectClass-mval (first args)))))]
     (if (= (length meta-startuple) 0)
       (some false-val) 
       (type-case CVal (first meta-startuple) 
                  [VObjectClass (a mval d class)
                                (if (truthy-object? (VObject a mval d)) 
                                    (some true-val) 
                                    (some false-val))]
                  [VUndefined () (some false-val)]
                  [else (error 'bool-init "Should not initialize boolean from pointer")]))))
