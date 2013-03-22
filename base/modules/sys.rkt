#lang plai-typed/untyped

(require "../python-core-syntax.rkt"
         "../util.rkt")

(define sys-module-name 'sys)
(define sys-id (CId sys-module-name (GlobalId)))
(define sys-module
  (seq-ops
   (list
    (CAssign sys-id (CObject (CId '$module (GlobalId)) (none)))
    (CAssign (CGetField sys-id 'path)
             (CList (CId '%list (GlobalId))
                    (map (lambda (x)
                           (make-builtin-str x))
                         (get-module-path))))
    (CAssign (CGetField sys-id '__name__)
             (make-builtin-str "sys"))
    (CAssign (CGetField sys-id 'exc_info)
             (CFunc (list) (none)
                    (CReturn (make-builtin-str "sys.exc_info"))
                    (none)))
    (CAssign (CGetField sys-id 'modules)
             (py-app (CId '%dict (GlobalId))
                   (list
                    (CList (CId '%list (GlobalId))
                           (list
                            (CTuple (CId '%tuple (GlobalId))
                              (list
                               (make-builtin-str "sys")
                               sys-id)))))
                   (none))))))
