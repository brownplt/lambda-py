#lang plai-typed/untyped

(require "python-core-syntax.rkt"
         "python-lexical-syntax.rkt"
         "python-syntax-operations.rkt")

(define (desugar-macros [expr : LexExpr])
  (lexexpr-modify-tree expr
    (lambda (expr)
      (type-case LexExpr expr
        [LexApp (fun args)
         (cond
          [(and (LexGlobalId? fun) (equal? (LexGlobalId-x fun) '___assign)
                (= (length args) 2) (LexStr? (first args)))
           (LexAssign
            (list (LexGlobalId (string->symbol (LexStr-s (first args))) 'Load))
            (second args))]
          [(and (LexGlobalId? fun) (equal? (LexGlobalId-x fun) '___id)
                (= (length args) 1) (LexStr? (first args)))
           (LexGlobalId (string->symbol (LexStr-s (first args))) 'Load)]
          [(and (LexGlobalId? fun) (equal? (LexGlobalId-x fun) '___emptyset)
                (= (length args) 0))
           (LexSet args)]
          [(and (LexGlobalId? fun) (equal? (LexGlobalId-x fun) '___delta)
                (> (length args) 0) (LexStr? (first args)))
           (LexBuiltinPrim (string->symbol (LexStr-s (first args))) (rest args))]
          [(and (LexGlobalId? fun) (equal? (LexGlobalId-x fun) '___getattr)
                (= (length args) 2))
           (LexExprField (first args) (second args))]
          [(and (LexGlobalId? fun) (equal? (LexGlobalId-x fun) '___prim2)
                (= (length args) 3) (LexStr? (first args)))
           (LexBinOp (second args)
                     (string->symbol (LexStr-s (first args)))
                     (third args))]
          [else expr])]
        [else (default-recur)]))))

