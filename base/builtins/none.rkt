#lang plai-typed

(require "../python-core-syntax.rkt"
         "../util.rkt"
         "str.rkt")


(define none-class
  (seq-ops (list 
             (CAssign (CId 'none (GlobalId))
                      (CClass
                        'none
                        (list 'object)
                        (CNone)))
             (def 'none '__str__
                  (CFunc (list 'self) (none)
                         (CReturn (make-builtin-str "None"))
                         (some 'none))))))

(define cnone
  (CNone))

(define vnone
  (VObject
    'none
    (some (MetaNone))
    (hash empty)))

