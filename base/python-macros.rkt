#lang plai-typed/untyped

(require "python-core-syntax.rkt"
         "python-lexical-syntax.rkt"
         "python-syntax-operations.rkt")

(define (desugar-macros expr)
  (lexexpr-modify-tree expr
    (lambda (expr)
      (type-case LexExpr expr
        [LexApp (fun args)
         (cond
          [(and (LexGlobalId? fun) (equal? (LexGlobalId-x fun) '___id)
                (> (length args) 0) (LexStr? (first args)))
           (LexLocalId (string->symbol (LexStr-s (first args))) 'Load)]
          [(and (LexGlobalId? fun) (equal? (LexGlobalId-x fun) '___delta)
                (> (length args) 0) (LexStr? (first args)))
           (LexBuiltinPrim (string->symbol (LexStr-s (first args))) (rest args))]
          [(and (LexGlobalId? fun) (equal? (LexGlobalId-x fun) '___prim2)
                (= (length args) 3) (LexStr? (first args)))
           (LexBinOp (second args)
                     (string->symbol (LexStr-s (first args)))
                     (third args))]
          [else expr])]
        [else (error 'desugar-macros "This should be caught")]))))

