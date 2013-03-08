#lang plai-typed/untyped

(require "../python-core-syntax.rkt"
         "../util.rkt")

(define sys-module-name 'sys)
(define sys-module
  (seq-ops
   (list
    (CAssign (CId sys-module-name (GlobalId))
             (CObject (CId '$module (GlobalId)) (none)))
    (CAssign (CGetField (CId sys-module-name (GlobalId)) 'path)
             (CList (CId '%list (GlobalId))
                    (list
                     (make-builtin-str ".")
                     (make-builtin-str "/home/ibs/lambda-py/tests/modules"))))
    (CAssign (CGetField (CId sys-module-name (GlobalId)) 'modules)
             (CDict
              (CId '%dict (GlobalId))
              (hash
               (list
                (values (make-builtin-str "sys")
                        ;;NOTE: this relies on aliasing!
                        (CId sys-module-name (GlobalId))))))))))
