#lang plai-typed

(require  "../../python-core-syntax.rkt"
          "../../util.rkt"
          "define-module.rkt")


; sys-module will be bound to $sys in python-lib
; when user import sys, __import__ will first check  $sys.modules,
; and return $sys.modules['sys']

(define-module sys-module
  '$sys
  ('path (CStr (get-import-path))) ;FIXME: should be CList
  ('modules (make-dict (list (CStr "sys") (CId '$sys (GlobalId)))
                       (list (CStr "imp") (CId '$imp (GlobalId))))))

