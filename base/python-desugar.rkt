#lang plai-typed

(require "python-syntax.rkt"
         "python-core-syntax.rkt"
         "util.rkt"
         "builtins/num.rkt" 
         "builtins/str.rkt")
(require (typed-in racket/base (number->string : (number -> string)))
         (typed-in racket/base (append : ((listof 'a) (listof 'a) -> (listof 'a))))
         (typed-in racket/base (cdr : (('a * 'b)  -> 'b)))
         (typed-in racket/base (car : (('a * 'b)  -> 'a)))
         (typed-in racket/list (last : ((listof 'a) -> 'a)))
         (typed-in racket/list (count : (('a -> boolean) (listof 'a) -> number)))
         (typed-in racket/list (take : ((listof 'a) number -> (listof 'a))))
         (typed-in racket/base (append : ((listof 'a) (listof 'a) -> (listof 'a)))))

(define (desugar-boolop [op : symbol] [values : (listof PyExpr)]
                        [global? : boolean]
                        [env : IdEnv]) : DesugarResult
  (begin ;(display "binop: ") (display values) (display "\n\n")
  (local [(define first-val (rec-desugar (first values) global? env false))]
    (if (> (length values) 1)
      (local [(define rest-val
                (desugar-boolop op (rest values) global? (DResult-env first-val)))]
        (case op
          ['And (DResult
                  (CIf (DResult-expr first-val)
                       (DResult-expr rest-val) 
                       (DResult-expr first-val))
                  (DResult-env rest-val))]
          ['Or (DResult
                 (CIf (DResult-expr first-val)
                      (DResult-expr first-val)
                      (DResult-expr rest-val))
                 (DResult-env rest-val))]))
    first-val))))

(define (desugar-compop [l : PyExpr] 
                        [ops : (listof symbol)] 
                        [comparators : (listof PyExpr)]
                        [global? : boolean]
                        [env : IdEnv]) : DesugarResult
  (begin ;(display "compop: ") (display comparators) (display "\n")
         ;(display ops) (display "\n")
         ;(display l) (display "\n")
         (local [;(define first-right (rec-desugar (first comparators) global? env))
                 ;(define l-expr (rec-desugar l global? (DResult-env first-right)))
                 (define first-comp (rec-desugar (PyBinOp l (first ops) (first
                                                                          comparators))
                                                 global? env false))]
                (if (> (length comparators) 1) 
                  (local [(define rest-comp (desugar-compop (first comparators)
                                                            (rest ops)
                                                            (rest comparators)
                                                            global?
                                                            (DResult-env first-comp)))]
                         (DResult
                           (CIf (DResult-expr first-comp)
                                (DResult-expr rest-comp)
                                (DResult-expr first-comp))
                           (DResult-env rest-comp)))
                  first-comp))))

;; look through a  and find a list of all names from assignments and definition
;; if global scope, it only gets definitions, for local scope it gets
;; definitions and assignments
(define (get-names [expr : PyExpr] [global? : boolean] [env : IdEnv]) : (listof symbol)
  (type-case PyExpr expr
   [PyIf (t b e) (append (get-names b global? env)
                         (get-names e global? env))]
   [PySeq (es) (foldl (lambda(e so-far) (append (get-names e global? env) so-far))
                      empty
                      es)]
   ; ignore special variables that we've defined in desugaring for which 
   ; we have specially constructed the scope manually for purposes of 
   ; desugaring
   [PyId (id ctx) (if (not (symbol=? ctx 'DesugarVar)) 
                    (list id)
                    empty)]
   [PyAssign (targets v) (begin
                           ;(display targets) (display "\n")  (display v) (display "\n\n")
                           (if (and (not global?) (not (PySubscript? (first targets))))
                           (foldl (lambda(t so-far) (append (get-names t global? env)
                                                            so-far))
                                  empty
                                  targets)
                           empty))]
   [PyExcept (t body) (get-names body global? env)]
   [PyTryExceptElseFinally (t e o f)
                           (append (get-names t global? env)
                              (append (foldl (lambda(e so-far) (append
                                                                 (get-names e
                                                                            global?
                                                                            env)
                                                                 so-far))
                                             empty 
                                             e)
                                (append (get-names o global? env)
                                  (get-names f global? env))))]
   [PyClass (name bases body) (list name)]
   [PyBinOp (l o r) (append (get-names l global? env)
                      (get-names r global? env))]
   [PyUnaryOp (o operand) (get-names operand global? env)]
   [PyFunc (name args defvargs body) (list name)]
   [PyClassFunc (name args body) (list name)]
   [PyFuncVarArg (name args sarg body) (list name)]
   [else empty]))

(define (get-right-names [expr : PyExpr] [global? : boolean] [env : IdEnv]) : (listof symbol)
  (type-case PyExpr expr
   [PyIf (t b e) (append (get-right-names b global? env)
                         (get-right-names e global? env))]
   [PySeq (es) (foldl (lambda(e so-far) (append (get-right-names e global? env) so-far))
                      empty
                      es)]
   ; ignore special variables that we've defined in desugaring for which 
   ; we have specially constructed the scope manually for purposes of 
   ; desugaring
   [PyId (id ctx) (if (not (symbol=? ctx 'DesugarVar)) 
                    (list id)
                    empty)]
   [PyAssign (targets v) (begin
                           ;(display targets) (display "\n")  (display v) (display "\n\n")
                           (if (not global?)
                               (get-right-names v global? env)
                               empty))]
   [PyExcept (t body) (get-right-names body global? env)]
   [PyTryExceptElseFinally (t e o f)
                           (append (get-right-names t global? env)
                              (append (foldl (lambda(e so-far) (append
                                                                 (get-right-names e
                                                                            global?
                                                                            env)
                                                                 so-far))
                                             empty 
                                             e)
                                (append (get-right-names o global? env)
                                  (get-right-names f global? env))))]
   [PyBinOp (l o r) (append (get-right-names l global? env)
                      (get-right-names r global? env))]
   [PyUnaryOp (o operand) (get-right-names operand global? env)]
   [else empty]))

(define (get-globals/nonlocals [expr : PyExpr] [global? : boolean]
                               [env : IdEnv]) : IdEnv
  (local [(define (rec-get-g/ns [exprs : (listof PyExpr)]
                                [global? : boolean]
                                [env : IdEnv]) : IdEnv
            (cond
              [(empty? exprs) env]
              [else (rec-get-g/ns (rest exprs) global?
                      (get-globals/nonlocals (first exprs) global? env))]))]
    (type-case PyExpr expr
      [PyIf (t b e) (get-globals/nonlocals e global? env)]
      [PySeq (es) (rec-get-g/ns es global? env)]
      [PyId (id ctx) (begin ;(display id) (display "\n")
                            ;(display env) (display "\n")
                     (local [(define type (lookup-idtype id env))]
                            (begin
                             ;(display "type: ") (display type) (display "\n\n")
                       (if (some? type)
                           env 
                           (add-id id (if global?
                                          (GlobalId)
                                          (LocalId))
                                   env)))))]
      [PyGlobal (ids) (begin ;(display "global: ") (display ids) (display "\n")
                      (add-ids ids (GlobalId) env))]
      [PyNonlocal (ids) (begin ;(display "nonlocal: ") (display ids) (display "\n")
                               ;(display env) (display "\n\n")
                        (add-ids ids (NonlocalId) env))]
      [PyAssign (targets v) (rec-get-g/ns targets global? env)]
      [PyAugAssign (o t v) (get-globals/nonlocals v global? 
                             (get-globals/nonlocals t global? env))]
      [PyExcept (t body) (get-globals/nonlocals body global? env)]
      [PyTryExceptElseFinally (t e o f)
                (get-globals/nonlocals f global?
                  (get-globals/nonlocals o global? 
                    (rec-get-g/ns e global? 
                      (get-globals/nonlocals t global? env))))]
      [PyClass (name bases body)
               (local [(define type (lookup-idtype name env))]
                      (if (some? type)
                          env
                          (add-id name (LocalId) env)))]
      [PyBinOp (l o r) (get-globals/nonlocals r global?
                         (get-globals/nonlocals l global? env))]
      [PyUnaryOp (o operand) (get-globals/nonlocals operand global? env)]
      [PyFunc (name args defargs body) (local [(define type (lookup-idtype name env))]
                                 (if (some? type)
                                     env 
                                     (add-id name (if global?
                                                      (GlobalId)
                                                      (LocalId))
                                             env)))]
      [PyFuncVarArg (name args sarg body)
                    (local [(define type (lookup-idtype name env))]
                      (if (some? type)
                          env
                          (add-id name (if global?
                                           (GlobalId)
                                           (LocalId))
                                  env)))]
      [else env])))


(define (desugar-pymodule [es : (listof PyExpr)] 
                          [global? : boolean]
                          [env : IdEnv]) : DesugarResult
  (local [(define g/ns-env (get-globals/nonlocals (PySeq es) global? env))
          (define names  (get-names (PySeq es) global? g/ns-env))
          (define prelude 
            (if (not (empty? names))
              (rec-desugar (PySeq (map (lambda (n) (PyAssign (list (PyId n 'Load))
                                                             (PyUndefined)))
                                       names))
                           global? g/ns-env false)
              (rec-desugar (PyPass) global? g/ns-env false)))
          (define body (rec-desugar (PySeq es) global? (DResult-env prelude) false))]
    (DResult
      (CModule
        (DResult-expr prelude)
        (DResult-expr body))
      (DResult-env body))))

(define (map-desugar [exprs : (listof PyExpr)]
                     [global? : boolean]
                     [env : IdEnv] [inclass? : boolean]): ((listof CExpr) * IdEnv)
  (local [(define (rec-map-desugar exps g e)
            (cond
              [(empty? exps) (values empty e)]
              [(cons? exps)
               (local [(define first-r (rec-desugar (first exps) g e inclass?))
                       (define-values (results last-env)
                         (rec-map-desugar (rest exps) g (DResult-env first-r)))]
                 (values
                   (cons (DResult-expr first-r)
                         results)
                   last-env))]))]
    (rec-map-desugar exprs global? env)))

(define (assign-undef [s : symbol]) : PyExpr
  #|
  this seems like a good idea but it breaks a test so i don't know
  (PyTryExceptElseFinally 
    (PyId s 'Load)
    (list 
      (PyExcept (list (PyId 'NameError 'Load))
                (PyAssign (list (PyId s 'Load))
                          (PyUndefined))))
    (PyPass)
    (PyPass)))|#
    (PyAssign (list (PyId s 'Load)) (PyUndefined)))

(define (assign-val [s : symbol]) : PyExpr
  (PyAssign (list (PyId s 'Load)) (PyId s 'Load)))

;; for the body of some local scope level like a class or function, hoist
;; all the assignments and defs to the top as undefineds
(define (desugar-local-body [expr : PyExpr] [args : (listof symbol)]
                            [env : IdEnv]) : DesugarResult
  (local [(define g/ns-env (get-globals/nonlocals expr false empty))
          (define names (get-names expr false g/ns-env))
          (define right-names (filter (λ (n) (not (member n names)))
                                         (get-right-names expr false g/ns-env)))]
(begin 
    (rec-desugar
      (if (not (empty? names))
          (PySeq (append 
                   (map assign-undef
                        (filter (lambda(n) (not (member n args)))
                                names))
                   (append (map assign-val right-names)
                           (list expr))))
          expr)
      false
      g/ns-env
      false))))

(define (desugar-for [target : PyExpr] [iter : PyExpr] [body : PyExpr]
                     [global? : boolean] [env : IdEnv]) : DesugarResult
  (local [(define iter-pyid (PyId (new-id) 'Load))]
     (rec-desugar
       (PySeq
         (list (PyAssign (list iter-pyid) (PyApp (PyId 'iter 'Load) (list iter)))
           (PyWhile (PyBool true)
                  (PySeq 
                    (list 
                       (PyAssign (list target) (PyNone))
                       (PyTryExceptElseFinally

                           (PyAssign (list target) 
                                     (PyApp (PyDotField iter-pyid '__next__) empty))

                           (list (PyExcept (list (PyId 'StopIteration 'Load))
                                           (PyBreak)))
                           (PyPass)
                           (PyPass))
                       body))
                       (PyPass))))
       global?
       env
       false)))

(define (desugar-listcomp [body : PyExpr] [gens : (listof PyExpr)] 
                          [global? : boolean] [env : IdEnv]) : DesugarResult
  (local [(define list-id (PyId (new-id) 'Load))
          (define (make-comploop gens)
            (cond 
              [(empty? gens) (PyApp (PyDotField list-id 'append) 
                                    (list body))]
              [(cons? gens)
               (PyFor (PyComprehen-target (first gens))
                      (PyComprehen-iter (first gens))
                      (make-comploop (rest gens)))]))
          (define full-expr
            (PySeq
              (list 
                  (PyAssign (list list-id) (PyList empty))
                  (make-comploop gens)
                  list-id)))]
         (rec-desugar full-expr global? env false)))



(define (rec-desugar [expr : PyExpr] [global? : boolean]
                     [env : IdEnv] [inclass? : boolean]) : DesugarResult 
  (begin ;(display expr) (display "\n\n")
    (type-case PyExpr expr
    [PySeq (es) (local [(define-values (exprs-r last-env)
                          (map-desugar es global? env inclass?))]
                  (DResult
                    (foldl (lambda (e1 so-far) (CSeq so-far e1))
                           (first exprs-r)
                           (rest exprs-r))
                    last-env))]
    [PyModule (es) (desugar-pymodule es global? env)]
    [PyAssign (targets value) 
              (type-case PyExpr (first targets) 
                ; We handle three kinds of assignments.
                ; An assignment to a subscript is desugared as a __setitem__ call.
                [PySubscript (left ctx slice)
                   (letrec ([desugared-target (rec-desugar left global? env false)]
                            [desugared-slice 
                              (rec-desugar slice global? (DResult-env desugared-target) false)]
                            [desugared-value
                              (rec-desugar value global? (DResult-env desugared-slice) false)]
                            [target-id (new-id)])
                     (DResult
                       (CApp (CGetField (DResult-expr desugared-target) '__setitem__)
                             (list (DResult-expr desugared-target)
                                   (DResult-expr desugared-slice)
                                   (DResult-expr desugared-value))
                             (none))
                       (DResult-env desugared-value)))]
                ; An assignment to a tuple is desugared as multiple __setitem__ calls.
                [PyTuple (vals)
                  (local [(define-values (targets-r mid-env) (map-desugar vals global?
                                                                          env false))
                          (define value-r (rec-desugar value global? mid-env false))
                          (define assigns
                            (map2 (λ (t n) 
                                     (CAssign t (CApp
                                                  (CGetField (CId '$tuple_result (LocalId)) 
                                                             '__getitem__)
                                                  (list (CId '$tuple_result (LocalId))
                                                        (make-builtin-num n))
                                                  (none))))
                                  targets-r
                                  (build-list (length targets-r) identity)))]
                    (DResult
                      (CLet '$tuple_result (DResult-expr value-r) 
                            (foldl (λ (a so-far) (CSeq so-far a))
                                   (first assigns) (rest assigns)))
                      (DResult-env value-r)))]

                ; The others become a CAssign.
                [else
                  (local [(define-values (targets-r mid-env)
                            (map-desugar targets global? env false))
                          (define value-r (rec-desugar value global? mid-env false))]
                         (DResult
                           (foldl (lambda (t so-far)
                                    (CSeq so-far (CAssign t (DResult-expr value-r))))
                                  (CAssign (first targets-r) (DResult-expr value-r))
                                  (rest targets-r))
                           (DResult-env value-r)))])]

    [PyNum (n) (DResult (make-builtin-num n) env)]
    [PySlice (lower upper step) (error 'desugar "Shouldn't desugar slice directly")]
    [PyBool (b) (DResult (if b (CTrue) (CFalse)) env)]
    [PyNone () (DResult (CNone) env)]
    [PyStr (s) (DResult (make-builtin-str s) env)]
    [PyId (x ctx) (local [(define type (lookup-idtype x env))]
                    (begin
                      ;(display "desugar: ") (display x) (display ", ")
                      ;(display type) (display "\n")
                      ;(display env) (display "\n\n")
                    (if (some? type)
                        (DResult (CId x (some-v type)) env)
                        (local [(define ty (if global?
                                               (GlobalId)
                                               (LocalId)))]
                          (DResult (CId x ty) (add-id x ty env))))))]
    [PyGlobal (ids) (DResult (CNone) env)]
    [PyNonlocal (ids) (DResult (CNone) env)]
    [PyUndefined () (DResult (CUndefined) env)]  

    [PyRaise (expr) (local [(define expr-r (if (PyId? expr)
                                             ;;handle the implicit construction case
                                             (rec-desugar (PyApp expr empty)
                                                          global? env false) 
                                             (rec-desugar expr global? env false)))]
                            (DResult
                              (CRaise 
                                (if (PyPass? expr)
                                    (none)
                                    (some (DResult-expr expr-r))))
                              (DResult-env expr-r)))]

    ; PyPass is an empty lambda
    [PyPass () (DResult (CApp (CFunc empty (none) (CNone) false) empty (none)) env)] 

    [PyIf (test body orelse)
          (local [(define test-r (rec-desugar test global? env false))
                  (define body-r (rec-desugar body global? (DResult-env test-r) false))
                  (define orelse-r (rec-desugar orelse global? (DResult-env body-r) false))]
          (DResult 
            (CIf (DResult-expr test-r)
                 (DResult-expr body-r)
                 (DResult-expr orelse-r))
            (DResult-env orelse-r)))]

    [PyBinOp (left op right)
             (local [(define left-r (rec-desugar left global? env false))
                     (define left-c (DResult-expr left-r))
                     (define right-r (rec-desugar right global? (DResult-env left-r) false))
                     (define right-c (DResult-expr right-r))] 
               (case op 
                 ['Add (DResult (CApp (CGetField left-c '__add__) 
                                      (list left-c right-c)
                                      (none))
                                (DResult-env right-r))]
                 ['Sub (DResult (CApp (CGetField left-c '__sub__) 
                                      (list left-c right-c)
                                      (none))
                                (DResult-env right-r))]
                 ['Mult (DResult (CApp (CGetField left-c '__mult__)
                                       (list left-c right-c)
                                       (none))
                                 (DResult-env right-r))]
                 ['Div (DResult (CApp (CGetField left-c '__div__)
                                      (list left-c right-c)
                                      (none))
                                (DResult-env right-r))]
                 ['FloorDiv (DResult (CApp (CGetField left-c '__floordiv__)
                                           (list left-c right-c)
                                           (none))
                                     (DResult-env right-r))]
                 ['Mod (DResult (CApp (CGetField left-c '__mod__)
                                      (list left-c right-c)
                                      (none))
                                (DResult-env right-r))]
                 ['BitAnd (DResult (CApp (CGetField left-c '__and__)
                                (list left-c right-c)
                                (none))
                                (DResult-env right-r))]
                 ['BitOr (DResult (CApp (CGetField left-c '__or__)
                               (list left-c right-c)
                               (none))
                               (DResult-env right-r))]
                 ['BitXor (DResult (CApp (CGetField left-c '__xor__)
                                (list left-c right-c)
                                (none))
                                (DResult-env right-r))]
                 ['Eq (DResult (CApp (CGetField left-c '__eq__)
                                     (list left-c right-c)
                                     (none))
                               (DResult-env right-r))]
                 ['Gt (DResult (CApp (CGetField left-c '__gt__)
                                     (list left-c right-c)
                                     (none))
                               (DResult-env right-r))]
                 ['Lt (DResult (CApp (CGetField left-c '__lt__)
                                     (list left-c right-c)
                                     (none))
                               (DResult-env right-r))]
                 ['LtE (DResult (CApp (CGetField left-c '__lte__)
                                      (list left-c right-c)
                                      (none))
                                (DResult-env right-r))]
                 ['GtE (DResult (CApp (CGetField left-c '__gte__)
                                      (list left-c right-c)
                                      (none))
                                (DResult-env right-r))]
                 ['NotEq (rec-desugar (PyUnaryOp 'Not (PyBinOp left 'Eq right)) 
                                      global? env false)]
                 ['In (DResult 
                        (CApp (CFunc (list 'self 'test) (none)
                                     (CSeq
                                       (CAssign (CId '__infunc__ (LocalId))
                                                (CGetField (CId 'self (LocalId))
                                                           '__in__))
                                       (CIf (CId '__infunc__ (LocalId))
                                            (CReturn
                                              (CApp
                                                (CId '__infunc__ (LocalId))
                                                (list (CId 'self (LocalId))
                                                      (CId 'test (LocalId)))
                                                (none)))
                                            (CApp (CId 'TypeError (LocalId))
                                                  (list (CObject
                                                          'str
                                                          (some (MetaStr 
                                                                  (string-append
                                                                    "argument of type '___'" 
                                                                    "is not iterable")))))
                                                  (none))))
                                     false)
                              (list right-c left-c)
                              (none))
                        (DResult-env right-r))]
                 ['NotIn (rec-desugar (PyUnaryOp 'Not (PyBinOp left 'In right))
                                      global? env false)]
                 [else (DResult
                         (CPrim2 op left-c right-c)
                         (DResult-env right-r))]))]

    [PyUnaryOp (op operand)
               (case op
                 ['USub (rec-desugar (PyBinOp (PyNum 0) 'Sub operand) global? env false)]
                 ['UAdd (rec-desugar (PyBinOp (PyNum 0) 'Add operand) global? env false)]
                 ['Invert (local [(define roperand (rec-desugar operand global? env false))]
                            (DResult 
                              (CApp (CGetField (DResult-expr roperand) '__invrt__)
                                (list (DResult-expr roperand))
                                (none))
                              (DResult-env roperand)))]
                 [else (local [(define roperand (rec-desugar operand global? env false))]
                              (DResult (CPrim1 op 
                                               (DResult-expr roperand)) 
                                       (DResult-env roperand)))])]

    [PyBoolOp (op values) (desugar-boolop op values global? env)]
              
    [PyCompOp (l op rights) (local [(define c (desugar-compop l op rights global? env))]
                              (begin ;(display c) (display "\n")
                                     c))]

    [PyListComp (elt gens) (desugar-listcomp elt gens global? env)]
    [PyComprehen (target iter) (error 'desugar "Can't desugar PyComprehen")]


    [PyLam (args body)
           (local [(define rbody (rec-desugar body global? env false))]
                  (DResult 
                    (CFunc args (none)
                         (CReturn                   
                           (DResult-expr rbody))
                         false)
                    (DResult-env rbody)))]
    
    [PyFunc (name args defargs body)
            (if (> (length defargs) 0)
              (local [(define last-arg (first (reverse args)))]
                (rec-desugar
                  ; assuming 1 defarg for now, generalize later
                    (PySeq 
                      (list
                        (PyAssign (list (PyId last-arg 'DesugarVar))
                                                  (first (reverse defargs)))
                    (PyFuncVarArg name empty
                            'stararg 
                            (PySeq
                              (list
                                  (PyIf (PyCompOp (PyApp
                                                    (PyDotField (PyId 'stararg 'Load)
                                                              '__len__)
                                                    empty)
                                                  (list 'Gt)
                                                  (list (PyNum 0)))
                                        (PyAssign (list (PyId last-arg
                                                              'DesugarVar))
                                                  (PySubscript (PyId 'stararg 'Load)
                                                               'Load
                                                               (PyNum 0)))
                                      (PyPass))
                                   body))))) 
                    global? 
                    env
                    false))

            (local [(define body-r (desugar-local-body body args env))]
             (DResult
               (CAssign (CId name (LocalId))
                        (CFunc args (none) (DResult-expr body-r) inclass?))
               env)))]

    ; a PyClassFunc is a method whose first argument should be the class rather than self
    [PyClassFunc (name args body)
            (local [(define body-r (desugar-local-body body args env))]
             (DResult
                     (CAssign (CId name (LocalId))
                              (CFunc args (none)
                                     ; We do this by, inside the function body,
                                     ; taking the first argument, which is "self",
                                     ; using that to look up the object's class, and then
                                     ; "overwriting" the first argument with that value.
                                     ; The result is that, in the function body, the first
                                     ; argument is the class, as expected.
                                     (CSeq (CAssign (CId (first args) (LocalId))
                                                    (CBuiltinPrim '$class
                                                                      (list (CId
                                                                              (first
                                                                                args)
                                                                              (LocalId)))))
                                           (DResult-expr body-r))
                                     inclass?))
               env))]

    [PyFuncVarArg (name args sarg body)
                  (local [(define body-r 
                            (desugar-local-body body (append args (list sarg)) env))]
                    (DResult
                            (CAssign (CId name (LocalId))
                                     (CFunc args (some sarg) (DResult-expr body-r) inclass?))
                      env))]

    [PyReturn (value)
              (local [(define value-r (rec-desugar value global? env false))]
                     (DResult (CReturn (DResult-expr value-r)) (DResult-env value-r)))]

    [PyDict (keys values) 
            (local [(define-values (keys-r mid-env) (map-desugar keys global? env false))
                    (define-values (values-r last-env)
                      (map-desugar values global? mid-env false))]
              (DResult (CDict (lists->hash keys-r values-r)) last-env))]

    [PySet (elts) (local [(define-values (results last-env)
                            (map-desugar elts global? env false))]
                    (DResult (CSet results) last-env))]

    [PyList (values) (local [(define-values (results last-env)
                               (map-desugar values global? env false))]
                       (DResult (CList results) last-env))]

    [PyTuple (values) (local [(define-values (results last-env)
                                (map-desugar values global? env false))]
                        (DResult (CTuple results) last-env))]

    [PySubscript (left ctx slice)
      (cond
        [(symbol=? ctx 'Load)
         (local [(define left-id (new-id))
                 (define left-r (rec-desugar left global? env false))]
                (if (PySlice? slice)
                  (local [(define slice-low (rec-desugar (PySlice-lower slice)
                                                         global? env false))
                          (define slice-up (rec-desugar (PySlice-upper slice)
                                                        global? env false))
                          (define slice-step (rec-desugar (PySlice-step slice)
                                                          global? env false))]
                    (DResult
                      (CLet left-id
                            (DResult-expr left-r)
                            (CApp (CGetField (CId left-id (if global? (GlobalId) (LocalId)))
                                             '__slice__)
                                  (list 
                                    (CId left-id (if global? (GlobalId) (LocalId)))
                                    (DResult-expr slice-low)
                                    (DResult-expr slice-up)
                                    (DResult-expr slice-step))
                                  (none)))
                      (DResult-env slice-step)))
                  (local [(define slice-r (rec-desugar slice global?
                                                       (DResult-env left-r) false))] 
                         (DResult 
                    (CLet left-id 
                          (DResult-expr left-r)
                          (CApp (CGetField (CId left-id (if global? (GlobalId) (LocalId)))
                                           '__getitem__)
                                (list (CId left-id (if global? (GlobalId) (LocalId)))
                                      (DResult-expr slice-r))
                                (none)))
                    (DResult-env slice-r)))))]
        [(symbol=? ctx 'Store)
         (error 'desugar "bad syntax: PySubscript has context 'Store' outside a PyAssign")]
        [else (error 'desugar "unrecognized context in PySubscript")])]

    [PyBreak () (DResult (CBreak) env)]

    ;; very hacky solution for assertRaises: it needs laziness built into it, so instead
    ;; of defining it as a function, we'll special case it as a macro.
    [PyApp (fun args)
           (cond
             [(and (PyId? fun) (symbol=? (PyId-x fun) '___assertRaises))
               (local [(define f (rec-desugar (second args) global? env false))
                       (define-values (as as-env)
                         (map-desugar (rest (rest args)) global? (DResult-env f) false))
                       (define exns (rec-desugar (first args) global? as-env false))
                       (define pass (rec-desugar (PyPass) global? (DResult-env exns) false))]
                 (DResult
                   (CApp
                     (CFunc empty (none)
                            (CTryExceptElseFinally
                              (CApp (DResult-expr f) as (none))
                              (list
                                (CExcept (list (DResult-expr exns)) (none) (DResult-expr pass))
                                (CExcept empty (none) (DResult-expr pass)))
                              (CApp (CId 'print (GlobalId))
                                    (list (make-builtin-str "Assert failure!"))
                                    (none))
                              (DResult-expr pass))
                            false)
                     empty
                     (none))
                   (DResult-env pass)))]
             [(and (PyId? fun) (symbol=? (PyId-x fun) 'locals))
                (DResult
                  (CBuiltinPrim '$locals empty)
                  env)]
             [else
               (local [(define f (rec-desugar fun global? env false))
                       (define f-expr (DResult-expr f))
                       (define-values (results last-env)
                         (map-desugar args global? (DResult-env f) false))]
                 (DResult
                   (cond
                     [(CGetField? f-expr)
                      (local [(define o (CGetField-value f-expr))]
                        (CApp f-expr (cons o results) (none)))]
                     ; special case: "super" application gets extra 'self' argument
                     [(and (CId? f-expr) (symbol=? 'super (CId-x f-expr)))
                      (CApp f-expr (cons (CId 'self (LocalId)) results) (none))]
                     [else (CApp f-expr results (none))])
                   last-env))])]

    [PyAppStarArg (fun args sarg)
           (local [(define f (rec-desugar fun global? env false))
                   (define-values (results mid-env)
                     (map-desugar args global? (DResult-env f) false))
                   (define s (rec-desugar sarg global? mid-env false))]
             (DResult
               (if (CGetField? (DResult-expr f))
                 (local [(define o (CGetField-value (DResult-expr f)))]
                   (CApp (DResult-expr f) (cons o results) (some (DResult-expr s))))
                 (CApp (DResult-expr f) results (some (DResult-expr s))))
               (DResult-env s)))]
    
    [PyClass (name bases body)
             (local [(define newenv (get-globals/nonlocals body false env))
                     (define names (get-names body false newenv))
                     (define body-r (rec-desugar body false newenv true))
                     (define modbody
                       (if (member '__init__ names)
                           (DResult-expr body-r)
                           (CSeq (DResult-expr body-r)
                                 (CAssign 
                                   (CId '__init__ (LocalId))
                                   (CFunc (list 'self) (none)
                                          (CAssign 
                                            (CGetField
                                              (CId 'self (LocalId))
                                              '__class__)
                                            (CBuiltinPrim '$class
                                                          (list (CId 'self (LocalId)))))
                                          true)))))
                     (define modenv 
                       (if (member '__init__ names)
                           (DResult-env body-r)
                           (add-id '__init__ (LocalId) (DResult-env body-r))))]
               (DResult
                 (CAssign (CId name (if global? (GlobalId) (LocalId)))
                          (CClass name
                                  (if (empty? bases)
                                    'object
                                    (first bases))
                                  modbody))
                 modenv))]
    
    [PyDotField (value attr)
                (local [(define value-r (rec-desugar value global? env false))]
                   (DResult
                     (CGetField (DResult-expr value-r) attr)
                     (DResult-env value-r)))]
    
    [PyTryExceptElseFinally (try excepts orelse finally)
                (local [(define try-r (rec-desugar try global? env false))
                        (define-values (excepts-r mid-env)
                          (map-desugar excepts global? (DResult-env try-r) false))
                        (define orelse-r (rec-desugar orelse global? mid-env false))
                        (define finally-r (rec-desugar finally global?
                                                       (DResult-env orelse-r) false))]
                  (DResult
                    (CTryExceptElseFinally 
                      (DResult-expr try-r)
                      excepts-r
                      (DResult-expr orelse-r)
                      (DResult-expr finally-r))
                    (DResult-env finally-r)))]
    
    [PyExcept (types body)
              (local [(define-values (types-r mid-env)
                        (map-desugar types global? env false))
                      (define body-r (rec-desugar body global? mid-env false))]
                (DResult
                  (CExcept types-r 
                           (none)
                           (DResult-expr body-r))
                  (DResult-env body-r)))]
    
    [PyWhile (test body orelse)
             (local [(define test-r (rec-desugar test global? env false))
                     (define body-r (rec-desugar body global? (DResult-env test-r) false))
                     (define orelse-r (rec-desugar orelse global? (DResult-env body-r) false))]
             (DResult 
                 (CWhile (DResult-expr test-r)
                         (DResult-expr body-r)
                         (DResult-expr orelse-r))
                 (DResult-env orelse-r)))]
    [PyFor (target iter body) (desugar-for target iter body global? env)]

    [PyExceptAs (types name body)
                (local [(define-values (types-r mid-env)
                          (map-desugar types global? env false))
                        (define body-r (rec-desugar body global? mid-env false))]
                  (DResult
                    (CExcept types-r
                             (some name)
                             (DResult-expr body-r))
                    (DResult-env body-r)))]

    [PyAugAssign (op target value)
                 (local [(define target-r (rec-desugar target global? env false))
                         (define aug-r (rec-desugar (PyBinOp target op value)
                                                       global? (DResult-env target-r) false))]
                 (DResult
                   (CAssign (DResult-expr target-r)
                          (DResult-expr aug-r))
                 (DResult-env aug-r)))]
    ; XXX: target is interpreted twice, independently.
    ; Is there any case where this might cause problems?

    [PyDelete (targets)
              (let ([target (first targets)]) ; TODO: handle deletion of more than one target
                (type-case PyExpr target
                  [PySubscript (left ctx slice)
                    (letrec ([desugared-target (rec-desugar left global? env false)]
                             [desugared-slice
                               (rec-desugar slice global? (DResult-env desugared-target) false)]
                             [target-id (new-id)])
                      (DResult
                        (CLet target-id (DResult-expr desugared-target)
                              (CApp (CGetField (CId target-id (if global? (GlobalId) (LocalId)))
                                               '__delitem__)
                                    (list (CId target-id (if global? (GlobalId) (LocalId)))
                                          (DResult-expr desugared-slice))
                                    (none)))
                        (DResult-env desugared-slice)))]
                  [else (error 'desugar "We don't know how to delete identifiers yet.")]))]
)))

(define (desugar [expr : PyExpr]) : CExpr
  (type-case DesugarResult (rec-desugar expr true empty false)
    [DResult (expr env) expr]))

(define-type DesugarResult
   [DResult (expr : CExpr) (env : IdEnv)])

(define (lookup-idtype [id : symbol] [env : IdEnv]) : (optionof IdType)
  (local [(define matches (filter (lambda (p) (symbol=? (idpair-id p) id)) env))]
    (cond
      [(empty? matches) (none)]
      [(= 1 (count (lambda (p) (GlobalId? (idpair-type p))) matches)) (some (GlobalId))]
      [(= 1 (count (lambda (p) (NonlocalId? (idpair-type p))) matches)) (some (NonlocalId))]
      [else (some (LocalId))])))

(define (extract-globals [env : IdEnv]) : IdEnv
  (filter (lambda (p) (GlobalId? (idpair-type p))) env))

(define (merge-globals [complete-env : IdEnv] [only-globals : IdEnv])
  (local [(define globals (extract-globals only-globals))
          (define envs (map (lambda (g) (cons g complete-env)) globals))]
    (if (cons? envs)
        (last envs)
        complete-env)))

(define (add-ids [ids : (listof symbol)] [type : IdType] [env : IdEnv]) : IdEnv 
  (local [(define envs (map (lambda (id) (add-id id type env)) ids))]
    (last envs)))

(define (add-id [id : symbol] [type : IdType] [env : IdEnv]) : IdEnv
  (local [(define new-env (cons (idpair id type) env))]
    new-env))
