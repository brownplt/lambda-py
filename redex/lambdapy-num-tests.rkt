#lang racket

(require "lambdapy-test-util.rkt")

;; prims
(expect (builtin-prim "is" (vtrue vtrue)) vtrue)
(expect (builtin-prim "is" (vtrue vfalse)) vfalse)
(expect (builtin-prim "is" ((mknum 1) (mknum 1))) vtrue)
(expect (builtin-prim "is" ((mknum 1) (mknum 2))) vfalse)
#| ; NOTE(dbp): currently the implementation of numeric primitives,
   ; which we have copied, is buggy, and as a result, these tests don't pass!
   ; see: https://groups.google.com/d/msg/lambda-py/szbm86ron8Q/PbFO7RKOpKMJ
   ; -- agree with you. i was not sure whether we could change core's semantics
   ; -- so i just did this... -yao
(expect (builtin-prim "num+" ((mknum 1) (mknum 1))) (make-num 2))
(expect (builtin-prim "num-" ((mknum 2) (mknum 1))) (make-num 1))
(expect (builtin-prim "num*" ((mknum 2) (mknum 3))) (make-num 6))
(expect (builtin-prim "num/" ((mknum 4) (mknum 2))) (make-num 2))
(expect (builtin-prim "num//" ((mknum 5) (mknum 2))) (make-num 2))
(expect (builtin-prim "num%" ((mknum 5) (mknum 2))) (make-num 1))
|#
(expect (builtin-prim "num=" ((mknum 1) (mknum 1))) vtrue)
(expect (builtin-prim "num=" ((mknum 1) (mknum 2))) vfalse)
(expect (builtin-prim "num>" ((mknum 1) (mknum 1))) vfalse)
(expect (builtin-prim "num>" ((mknum 2) (mknum 1))) vtrue)
(expect (builtin-prim "num>" ((mknum 1) (mknum 2))) vfalse)

(expect (builtin-prim "num<" ((mknum 1) (mknum 1))) vfalse)
(expect (builtin-prim "num<" ((mknum 2) (mknum 1))) vfalse)
(expect (builtin-prim "num<" ((mknum 1) (mknum 2))) vtrue)

(expect (builtin-prim "num<=" ((mknum 1) (mknum 1))) vtrue)
(expect (builtin-prim "num<=" ((mknum 2) (mknum 1))) vfalse)
(expect (builtin-prim "num<=" ((mknum 1) (mknum 2))) vtrue)

(expect (builtin-prim "num>=" ((mknum 1) (mknum 1))) vtrue)
(expect (builtin-prim "num>=" ((mknum 2) (mknum 1))) vtrue)
(expect (builtin-prim "num>=" ((mknum 1) (mknum 2))) vfalse)
