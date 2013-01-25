#lang plai-typed

(require "../python-core-syntax.rkt"
         "../util.rkt"
         "str.rkt"
         "tuple.rkt")

(define code-class : CExpr
  (CClass
   '$code
   (list 'object)
   (seq-ops (list 
              (def '$code '__str__
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'code-str
                                                     (list (CId 'self (LocalId)))))
                          true))
              
              ; this should really be a property with name co_names
              (def '$code 'get_names
                   (CFunc (list 'self) (none)
                          (CReturn (CBuiltinPrim 'code-names
                                                     (list (CId 'self (LocalId)))))
                          true))
              
              ))))

(define (code-str (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$code
               (some (VObject 'str 
                        (some (MetaStr
                                (pretty-metaval mval1 sto)))
                        (make-hash empty)))))

(define (code-names (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto '$code
               (some (make-builtin-tuple (map (lambda (name) (make-str-value (symbol->string name)))
                                              (MetaCode-names mval1))))))
