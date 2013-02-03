#lang plai-typed

(require "../python-core-syntax.rkt"
         "../util.rkt"
         "str.rkt"
         "tuple.rkt")

(define code-class : CExpr
  (seq-ops
   (list
    (CAssign (CId '$code (GlobalId))
             (CClass '$code (list 'object) (CNone)))
    (def '$code 'get_globals
         (CFunc (list 'self) (none)
                (CReturn
                 (CBuiltinPrim 'code-globals
                               (list (CId 'self (LocalId)))))
                true))
    (def '$code '__str__
         (CFunc (list 'self) (none)
                (CReturn
                 (CBuiltinPrim 'code-str
                               (list (CId 'self (LocalId)))))
                true)))))

(define (code-str [args : (listof CVal)]
                  [env : Env]
                  [sto : Store]) : (optionof CVal)
  (check-types args env sto '$code
               (some (VObject 'str 
                        (some (MetaStr
                                (pretty-metaval mval1 sto)))
                        (hash empty)))))

(define (code-globals [args : (listof CVal)]
                      [env : Env]
                      [sto : Store]) : (optionof CVal)
  (check-types args env sto '$code
               (some (make-builtin-tuple
                      (map
                       (lambda (name)
                         (make-str-value (symbol->string name)))
                       (MetaCode-globals mval1))))))
