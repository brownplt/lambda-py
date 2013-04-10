#lang plai-typed/untyped

;; function class
(require "../python-core-syntax.rkt" 
         "../util.rkt"
         "type.rkt"
         "num.rkt"
         "str.rkt")

;; func-args: get a list of arguments, as strings.
;; first argument is the function object, second list class object, third str class object.
(define (func-args (args : (listof CVal)) env sto) : (optionof CVal)
  (if (= (length args) 3)
      (type-case CVal (first args)
        [VObjectClass (_ opt-mval __ ___)
          (type-case (optionof MetaVAl) opt-mval
            [some (mval)
              (type-case MetaVal mval
                [MetaClosure (_ func-args __ ___ ____)
                  (some (VObjectClass
                         'list
                         (some (MetaList (map (lambda (k)
                                                (make-str-value (symbol->string k)
                                                                (third args)))
                                              func-args)))
                         (hash empty)
                         (some (second args))))]
                [else (none)])]
            [none () (none)])]
        [else (none)])
      (none)))