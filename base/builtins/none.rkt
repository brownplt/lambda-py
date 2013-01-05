#lang plai-typed

(require "../python-core-syntax.rkt"
         "../util.rkt"
         "str.rkt")


(define none-class
  (CClass
    'none
    'object
    (seq-ops (list 
               (def '__str__
                    (CFunc (list 'self) (none)
                           (CReturn (make-builtin-str "None"))
                           true))))))
(define cnone
  (CObject 
    'none
    (some (MetaNone))))

(define vnone
  (VObject
    'none
    (some (MetaNone))
    (make-hash empty)))

