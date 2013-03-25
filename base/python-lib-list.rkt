#lang racket

(require racket/runtime-path)
(provide python-libs)

(define-runtime-path-list python-libs '(
 "pylib/type.py"
 "pylib/none.py"
 "pylib/bool.py"
 "pylib/str.py"
 "pylib/tuple.py"
 "pylib/list.py"
 "pylib/dict.py"
 "pylib/set.py"
 "pylib/function.py"
 "pylib/method.py"
 "pylib/super.py"
 "pylib/range.py"
 "pylib/seq_iter.py"
 "pylib/print.py"
 "pylib/filter.py"
 "pylib/any.py"
 "pylib/all.py"
 "pylib/import.py"
 "pylib/file.py"
 "pylib/isinstance.py"
 "pylib/callable.py"
 "pylib/generator.py"
 "pylib/object.py"
 "pylib/attr.py"
 "pylib/property.py"
 "py-prelude.py"))
  
