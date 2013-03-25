#lang plai-typed/untyped

(require "../python-core-syntax.rkt"
         "main-module.rkt"
         "../util.rkt")

(define sys-module-name 'sys)
(define sys-id (CId sys-module-name (GlobalId)))
(define sys-module
  (seq-ops
   (list
    (CAssign sys-id (CObject (CId '$module (GlobalId)) (none)))
    (set-field sys-id 'path
             (CList (CId '%list (GlobalId))
                    (map (lambda (x)
                           (make-builtin-str x))
                         (get-module-path))))
    (set-field sys-id '__name__
             (make-builtin-str "sys"))
    (set-field sys-id 'exc_info
             (CFunc (list) (none)
                    (CReturn (make-builtin-str "sys.exc_info"))
                    (none)))
    (set-field sys-id 'modules
             (py-app (CId '%dict (GlobalId))
                   (list
                    (CList (CId '%list (GlobalId))
                           (list
                            (CTuple (CId '%tuple (GlobalId))
                              (list
                               (make-builtin-str "sys")
                               sys-id))
                            (CTuple (CId '%tuple (GlobalId))
                              (list
                               (make-builtin-str (symbol->string main-module-name))
                               main-module-id)))))
                   (none))))))
