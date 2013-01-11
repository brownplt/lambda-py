#lang plai-typed

(require  "../../python-core-syntax.rkt"
          "../../util.rkt"
          "../str.rkt")


(define sys-class
  (CClass
   '$sys-class
   (list '$module)
   (seq-ops (list
             (def '__init__
                  ; self.path = init-path
                  ; self.modules = ["sys" : $sys], in this case, 
                  ; $sys is bound to sys in environment
                  (CFunc (list 'self) (none)
                         (CSeq
                          (CAssign
                           (CGetField (CId 'self (LocalId)) 'path)
                           (CObject 'str (some (MetaStr (get-import-path)))))
                          (CAssign
                           (CGetField (CId 'self (LocalId)) 'modules)
                           (CDict (hash 
                                   (list 
                                    (values
                                     (CObject 'str (some (MetaStr "sys"))) (CId '$sys (GlobalId))))))))
                         true))
             ))))



; sys-module should be bound to $sys in python-lib
; when user import sys, __import__ will first check  $sys.modules,
; and return $sys.modules['sys']
(define sys-module
  (CApp (CId '$sys-class (GlobalId))
        (list)
        (none)))
