#lang plai-typed/untyped

(require "../python-core-syntax.rkt")
(require "../util.rkt"
         "set.rkt"
         "str.rkt")
(require
  (typed-in racket/base (hash->list : ((hashof 'a 'b)  -> (listof 'c))))
  (typed-in racket/base (car : (('a * 'b)  -> 'a)))
  (typed-in racket/base (cdr : (('a * 'b)  -> 'b)))
)

(define (make-under-dict [h : (hashof symbol Address)] [env : Env] [sto : Store]) : Result
  (local [(define filledhash (make-hash empty))
          (define new-sto sto)
          (define dicthash
            (map (λ (pair)
                    (let ([res (alloc-result
                                 (make-str-value (symbol->string (car pair)) (fetch-once (some-v (lookup '%dict env)) new-sto))
                                 new-sto)])
                      (begin
                        (hash-set! filledhash
                                   (v*s-v res)
                                   (fetch-once (cdr pair) (v*s-s res)))
                        (set! new-sto (v*s-s res)))))
                    (hash->list h)))]
    (alloc-result
      (VObjectClass 'dict
                    (some (MetaDict filledhash))
                    (hash empty)
                    (some (fetch-once (some-v (lookup '%dict env)) new-sto)))
      new-sto)))

