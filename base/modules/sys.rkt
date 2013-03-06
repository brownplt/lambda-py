#lang plai-typed/untyped

(require "../python-core-syntax.rkt"
         "../util.rkt")

(define sys-module
  (let ((bound-to 'sys))
    (seq-ops
     (list
      (CAssign (CId bound-to (GlobalId))
               (CObject (CId '$module (GlobalId)) (none)))
      (CAssign (CGetField (CId bound-to (GlobalId)) 'path)
               (CList (CId '%list (GlobalId))
                      (list
                       (CObject (CId '%str (GlobalId))
                                (some (MetaStr "sys.path1")))
                       (CObject (CId '%str (GlobalId))
                                (some (MetaStr "sys.path2"))))))
      (CAssign (CGetField (CId bound-to (GlobalId)) 'modules)
               (CDict
                (CId '%dict (GlobalId))
                (hash
                 (list
                  (values (CObject (CId '%str (GlobalId))
                                   (some (MetaStr "sys")))
                          (CId bound-to (GlobalId)))))))))))
