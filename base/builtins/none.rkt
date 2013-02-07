#lang plai-typed

(require "../python-core-syntax.rkt"
         "../util.rkt"
         "str.rkt")


(define none-class
<<<<<<< HEAD
  (seq-ops (list 
             (CAssign (CId 'none (GlobalId))
                      (CClass
                        'none
                        (list 'object)
                        (CNone)))
             (def 'none '__str__
                  (CFunc (list 'self) (none)
                         (CReturn (make-builtin-str "None"))
                         true)))))

=======
  (CClass
    'none
    (list 'object)
    (seq-ops (list 
               (def '__str__
                    (CFunc (list 'self) (none)
                           (CReturn (make-builtin-str "None"))
                           (some 'none)))))))
>>>>>>> master
(define cnone
  (CObject 
    'none
    (some (MetaNone))))

(define vnone
  (VObject
    'none
    (some (MetaNone))
    (hash empty)))

