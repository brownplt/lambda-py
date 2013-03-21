#lang racket

(require racket/runtime-path)
(provide python-libs)

(define-runtime-path-list python-libs '(
 "pylib/none.py"
 "pylib/bool.py"
 "pylib/str.py"
 "pylib/tuple.py"
 "pylib/list.py"
 "pylib/dict.py"
 "pylib/set.py"
 "pylib/type.py"
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
 "py-prelude.py"))
  
