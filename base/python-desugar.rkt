#lang plai-typed/untyped

(require "python-lexical-syntax.rkt"
         "python-core-syntax.rkt"
         "util.rkt"
         "builtins/type.rkt"
         "builtins/num.rkt"
         "python-syntax-operations.rkt"
         "builtins/str.rkt")
(require (typed-in racket/base (number->string : (number -> string)))
         (typed-in racket/list (last : ((listof 'a) -> 'a)))
         (typed-in racket/list (count : (('a -> boolean) (listof 'a) -> number)))
         (typed-in racket/base (append : ((listof 'a) (listof 'a) -> (listof 'a)))))


(define (desugar-boolop [op : symbol] [values : (listof LexExpr)]) : CExpr
  (local [(define first-val (rec-desugar (first values)))]
    (if (> (length values) 1)
        (local [(define rest-val (desugar-boolop op (rest values)))]
          (case op
            ['And (CIf first-val
                       rest-val 
                       first-val)]
            ['Or (CIf first-val
                      first-val
                      rest-val)]))
        first-val)))

(define (desugar-compop [l : LexExpr] [ops : (listof symbol)] 
                        [comparators : (listof LexExpr)]) : CExpr
    (local [(define first-comp
              (rec-desugar (LexBinOp l (first ops) (first comparators))))]
      (if (> (length comparators) 1) 
          (local [(define rest-comp
                    (desugar-compop (first comparators)
                                    (rest ops)
                                    (rest comparators)))]
                 (CIf first-comp rest-comp first-comp))
          first-comp)))


(define (desugar-pymodule [es : (listof LexExpr)]) : CExpr
  (local [(define prelude (rec-desugar (LexPass)))
          (define body (rec-desugar (LexSeq es)))]
     (CModule prelude body)))


;; look through a  and find a list of all names from assignments and definition
;; if global scope, it only gets definitions, for local scope it gets
;; definitions and assignments

#|(define (desugar-pymodule [es : (listof PyExpr)] 
                          [global? : boolean]
                          [env : IdEnv]) : DesugarResult
  (local [(define g/ns-env (get-globals/nonlocals (PySeq es) global? env))
          (define names  (get-names (PySeq es) global? g/ns-env))
          (define prelude 
            (if (not (empty? names))
                (rec-desugar (PySeq (map (lambda (n) (PyAssign (list (PyId n 'Load))
                                                               (PyUndefined)))
                                         names))
                             global? g/ns-env (none))
                (rec-desugar (PyPass) global? g/ns-env (none))))
          (define body (rec-desugar (PySeq es) global? (DResult-env prelude) (none)))]
    (DResult
     (CModule
      (DResult-expr prelude)
      (DResult-expr body))
     (DResult-env body))))
|#

(define (desugar-for [target : LexExpr] [iter : LexExpr]
                     [body : LexExpr]) : CExpr
  (local [(define iter-id (new-id))]
    (rec-desugar
     (LexLocalLet iter-id (LexApp (LexGlobalId '%iter 'Load) (list iter))
                  (LexWhile (LexBool true)
                            (LexSeq
                             (list
                              (LexTryExceptElse
                               (LexAssign (list target)
                                          (LexApp (LexDotField (LexLocalId iter-id 'Load)
                                                               '__next__)
                                                  empty))
                               (list (LexExcept (list (LexGlobalId 'StopIteration 'Load))
                                                (LexBreak)))
                               (LexPass))
                              body))
                            (LexPass))))))

(define (desugar-listcomp [body : LexExpr] [gens : (listof LexExpr)] ) : CExpr
  (local [(define list-id (new-id))
          (define (make-comploop gens)
            (cond
              [(empty? gens) (LexApp (LexDotField (LexLocalId list-id 'Load)
                                                  'append)
                                     (list body))]
              [(cons? gens)
               (LexFor (LexComprehen-target (first gens))
                       (LexComprehen-iter (first gens))
                       (make-comploop (rest gens)))]))
          (define full-expr
            (LexLocalLet list-id (LexList empty)
                         (LexSeq
                          (list
                           (make-comploop gens)
                           (LexLocalId list-id 'Load)))))]
         (rec-desugar full-expr)))

(define (which-scope [scp : LocalOrGlobal]) : IdType
  (type-case LocalOrGlobal scp
    [Locally-scoped () (LocalId)]
    [Globally-scoped () (GlobalId)]
    [Unknown-scope () (error 'desugar:
                             "somehow an Unknown-scope got past the lexical stage.")]
    [Instance-scoped () (error 'desugar:
                               "this is an instance variable, please handle it accordingly")]))

(define (desugar-seq [es : (listof LexExpr)]) : CExpr
  (cond
   [(empty? es) (error 'desugar "empty sequences are not supported")]
   [(empty? (rest es)) (desugar (first es))]
   [else (CSeq (desugar (first es)) (desugar-seq (rest es)))]))

(define (desugar-excepts [exn-id : symbol] [excepts : (listof LexExpr)]) : CExpr
  (local [(define (rec-desugar-excepts es)
            (cond
              [(empty? es) ;; exceptions other than $Reraise are reraised if not catched
                (rec-desugar (LexIf (LexApp (LexGlobalId '%isinstance 'Load)
                                            (list (LexLocalId exn-id 'Load)
                                                  (LexGlobalId '$Reraise 'Load)))
                                    (LexPass)
                                    (LexRaise (LexLocalId exn-id 'Load))))]
              [else
                (local [(define except (first es))
                        (define-values (body as types)
                          (if (LexExcept? except)
                              (values
                                (rec-desugar (LexExcept-body except))
                                (none)
                                (LexExcept-types except))
                              (values
                                (rec-desugar (LexExceptAs-body except))
                                (some (LexExceptAs-name except))
                                (LexExceptAs-types except))))
                        (define checks 
                          (cond
                            [(empty? types)
                             (list
                               (LexApp (LexGlobalId '%isinstance 'Load)
                                       (list (LexLocalId exn-id 'Load)
                                             (LexGlobalId 'BaseException 'Load))))]
                            [else (map (lambda (t)
                                         (LexApp (LexGlobalId '%isinstance 'Load)
                                                 (list (LexLocalId exn-id 'Load) t)))
                                       types)]))
                        (define condition (desugar-boolop 'Or checks))]
                  (CIf condition
                       (if (some? as)
                           (CLet (some-v as) (LocalId) (CId exn-id (LocalId)) body)
                           body)
                       (rec-desugar-excepts (rest es))))]))]
    (rec-desugar-excepts excepts)))

(define (id-to-symbol expr)
  (type-case LexExpr expr
    [LexLocalId (x ctx) x]
    [LexGlobalId (x ctx) x]
    [else (error 'desugar "cannot convert non-id to symbol with id-to-symbol")]))

(define (rec-desugar [expr : LexExpr] ) : CExpr 
  (begin ;(display expr) (display "\n\n")
    (type-case LexExpr expr
      [LexSeq (es) (desugar-seq es)]
      [LexModule (es) (desugar-pymodule es)]
      [LexAssign (targets value) 
                (type-case LexExpr (first targets) 
                  ; We handle three kinds of assignments.
                  ; An assignment to a subscript is desugared as a __setitem__ call.
                  [LexSubscript (left ctx slice)
                               (letrec ([desugared-target (rec-desugar left)]
                                        [desugared-slice 
                                         (rec-desugar slice)]
                                        [desugared-value
                                         (rec-desugar value)]
                                        [target-id (new-id)])
                                 (CApp (py-getfield desugared-target '__setitem__)
                                        (list 
                                              desugared-slice
                                              desugared-value)
                                        (none)))]
                  ; An assignment to a tuple is desugared as multiple __setitem__ calls.
                  [LexTuple (vals)
                           (local [(define targets-r (map rec-desugar vals))
                                   (define value-r (rec-desugar value))
                                   (define assigns
                                     (map2 (λ (t n) 
                                             (CAssign t (CApp
                                                         (py-getfield (CId '$tuple_result (LocalId)) 
                                                                    '__getitem__)
                                                         (list (make-builtin-num n))
                                                         (none))))
                                           targets-r
                                           (build-list (length targets-r) identity)))]
                              (CLet '$tuple_result (LocalId) value-r 
                                    (foldl (λ (a so-far) (CSeq so-far a))
                                           (first assigns) (rest assigns))))]
                  ; The others become a CAssign.
                  [else
                   (local [
                     (define (target-desugar target)
                      (type-case LexExpr target
                        [LexDotField (obj fld) (CGetField (rec-desugar obj) fld)]
                        [else (rec-desugar target)]))
                     (define targets-r (map target-desugar targets))
                     (define value-r (rec-desugar value))]
                          (foldl (lambda (t so-far)
                                   (CSeq so-far (CAssign t value-r)))
                                 (CAssign (first targets-r) value-r)
                                 (rest targets-r)))])]
      [LexNum (n) (make-builtin-num n)]
      [LexSlice (lower upper step) (error 'desugar "Shouldn't desugar slice directly")]
      [LexBool (b) (if b (CTrue) (CFalse))]
      [LexNone () (CNone)]
      [LexStr (s) (make-builtin-str s)]
      [LexLocalId (x ctx) (CId x (LocalId))]
      [LexGlobalId (x ctx) (CId x (GlobalId))]
      [LexGlobals (ids body)
                  (local 
                   [(define (make-global-lets ids body)
                    (cond
                     [(empty? ids) (rec-desugar body)]
                     [else (CLet (first ids) (GlobalId)
                           (rec-desugar (LexUndefined))
                           (make-global-lets (rest ids) body))]))]
                   (make-global-lets ids body))]
      [LexLocalLet (x bind body) (CLet x (LocalId) (rec-desugar bind) (rec-desugar body))]
      ;hopefully this will come in handy for whatever we decide to do with locals
      ;right now it's just a stub.
      [LexInScopeLocals (ids) (rec-desugar (LexPass))]
      [LexUndefined () (CUndefined)]  
      [LexRaise (expr) (local [(define expr-r
                                 (if (or (LexLocalId? expr) (LexGlobalId? expr))
                                     ;;handle the implicit construction case
                                     (rec-desugar (LexApp expr empty)) 
                                     (rec-desugar expr)))]

                         (CRaise 
                          (if (LexPass? expr)
                              (none)
                              (some expr-r))))]

      [LexYield (expr) (CYield (rec-desugar expr))]

      ;; assert check is always enabled, it doesn't test __debug__ builtin variable.
      [LexAssert (test msg)
                (rec-desugar
                 (LexIf test
                       (LexPass)
                       (LexRaise (LexApp (LexGlobalId 'AssertionError 'Load) msg))))]

      ; LexPass is an empty lambda
      [LexPass () (CApp (CFunc empty (none) (CNone) (none)) empty (none))] 
      [LexIf (test body orelse)
            (local [(define test-r (rec-desugar test))
                    (define body-r (rec-desugar body))
                    (define orelse-r (rec-desugar orelse))]
               (CIf test-r
                    body-r
                    orelse-r))]
      [LexBinOp (left op right)
               (local [(define left-r (rec-desugar left))
                       (define left-c left-r)
                       (define right-r (rec-desugar right))
                       (define right-c right-r)] 
                 (case op 
                   ['Add (CApp (py-getfield left-c '__add__) 
                               (list right-c)
                               (none))]
                   ['Sub (CApp (py-getfield left-c '__sub__) 
                               (list right-c)
                               (none))]
                   ['Mult (CApp (py-getfield left-c '__mult__)
                                (list right-c)
                                (none))]
                   ['Div (CApp (py-getfield left-c '__div__)
                               (list right-c)
                               (none))]
                   ['FloorDiv (CApp (py-getfield left-c '__floordiv__)
                                    (list right-c)
                                    (none))]
                   ['Mod (CApp (py-getfield left-c '__mod__)
                               (list right-c)
                               (none))]
                   ['BitAnd (CApp (py-getfield left-c '__and__)
                                  (list right-c)
                                  (none))]
                   ['BitOr (CApp (py-getfield left-c '__or__)
                                 (list right-c)
                                 (none))]
                   ['BitXor (CApp (py-getfield left-c '__xor__)
                                  (list right-c)
                                  (none))]
                   ['Eq (CApp (py-getfield left-c '__eq__)
                              (list right-c)
                              (none))]
                   ['Gt (CApp (py-getfield left-c '__gt__)
                              (list right-c)
                              (none))]
                   ['Lt (CApp (py-getfield left-c '__lt__)
                              (list right-c)
                              (none))]
                   ['LtE (CApp (py-getfield left-c '__lte__)
                               (list right-c)
                               (none))]
                   ['GtE (CApp (py-getfield left-c '__gte__)
                               (list right-c)
                               (none))]
                   ['NotEq (rec-desugar (LexUnaryOp 'Not (LexBinOp left 'Eq right)))]
                   ['In (CApp (CFunc (list 'container 'test) (none)
                                     (CLet '__infunc__ (LocalId)
                                           (py-getfield (CId 'container (LocalId))
                                                      '__in__)
                                           (CIf (CId '__infunc__ (LocalId))
                                                (CReturn
                                                  (CApp
                                                    (CId '__infunc__ (LocalId))
                                                    (list 
                                                          (CId 'test (LocalId)))
                                                    (none)))
                                                (CRaise (some
                                                  (CApp (CId 'TypeError (LocalId))
                                                        (list (make-builtin-str
                                                               (string-append
                                                                "argument of type '___'" 
                                                                "is not iterable")))
                                                        (none))))))
                                     (none))
                              (list right-c left-c)
                              (none))]
                   ['NotIn (rec-desugar (LexUnaryOp 'Not (LexBinOp left 'In right)))]
                   [else (CBuiltinPrim op (list left-c right-c))]))]

      [LexUnaryOp (op operand)
                  (case op
                    ['Not (CIf (CApp (CId '%bool (GlobalId)) (list (desugar operand)) (none)) (CFalse) (CTrue))]
                    ['USub (rec-desugar (LexBinOp (LexNum 0) 'Sub operand))]
                    ['UAdd (rec-desugar (LexBinOp (LexNum 0) 'Add operand))]
                    ['Invert (local [(define roperand (rec-desugar operand))]
                               (CApp (py-getfield roperand '__invrt__)
                                     (list)
                                     (none)))]
                    [else (CBuiltinPrim op (list (rec-desugar operand)))])]
      
      [LexBoolOp (op values) (desugar-boolop op values)]
      [LexCompOp (l op rights) (desugar-compop l op rights)]
      [LexListComp (elt gens) (desugar-listcomp elt gens)]
      [LexComprehen (target iter) (error 'desugar "Can't desugar LexComprehen")]
      
      [LexLam (args body) (CFunc args (none) (CReturn (rec-desugar body)) (none))]

      [LexFunc (name args defargs body decorators opt-class)
               (cond
                [(> (length defargs) 0 )
                   (local [(define last-arg (first (reverse args)))]
                     (rec-desugar
                       ; assuming 1 defarg for now, generalize later
                       ; NB: it also assumes no other arguments are present (fixed position
                       ; or stararg), otherwise they are silently discarded. (Alejandro).
                       (LexLocalLet last-arg
                                    (first (reverse defargs))
                           (LexFuncVarArg name empty
                                          'stararg 
                                          (LexSeq
                                            (list
                                              (LexIf (LexCompOp (LexApp
                                                                  (LexDotField
                                                                    (LexLocalId 'stararg 'Load)
                                                                    '__len__)
                                                                  empty)
                                                                (list 'Gt)
                                                                (list (LexNum 0)))
                                                     (LexAssign (list (LexLocalId last-arg
                                                                                  'DesugarVar))
                                                                (LexSubscript
                                                                  (LexLocalId 'stararg 'Load)
                                                                  'Load
                                                                  (LexNum 0)))
                                                     (LexPass))
                                              body))
                                          decorators opt-class))))]
                 [(empty? decorators)
                   (local [(define body-r (rec-desugar body))] 
                     (CFunc args (none) body-r (option-map id-to-symbol opt-class)))]

                 [else
                  (rec-desugar ;; apply decorators to the function
                   (foldr (lambda (decorator func) (LexApp decorator (list func)))
                          (LexFunc name args (list) body (list) opt-class)
                          decorators))])]

      [LexFuncVarArg (name args sarg body decorators opt-class)
                     (if (empty? decorators)
                         (CFunc args (some sarg) (rec-desugar body) (option-map id-to-symbol opt-class))
                         (rec-desugar ;; apply decorators to the function
                          (foldr (lambda (decorator func) (LexApp decorator (list func)))
                                 (LexFuncVarArg name args sarg body (list) opt-class)
                                 decorators)))]

      [LexReturn (value) (CReturn (rec-desugar value))]
      
      [LexDict (keys values)
       (local [
        (define (pairs->tupleargs keys values)
          (cond
            [(empty? keys) empty]
            [(cons? keys)
             (cons (CTuple (CId '%tuple (GlobalId))
                           (list (rec-desugar (first keys))
                                 (rec-desugar (first values))))
                   (pairs->tupleargs (rest keys) (rest values)))]))
        ]
        (CApp (CId '%dict (GlobalId))
          (list
            (CList (CId '%list (GlobalId))
                   (pairs->tupleargs keys values)))
          (none)))]
      [LexSet (elts) (CSet (CId '%set (GlobalId)) (map rec-desugar elts))]
      [LexList (values) (CList (CId '%list (GlobalId)) (map rec-desugar values))]
      [LexTuple (values) (CTuple (CId '%tuple (GlobalId)) (map rec-desugar values))]
      
      [LexSubscript (left ctx slice)
                    (cond
                      [(symbol=? ctx 'Load)
                       (local [(define left-id (new-id))
                               (define left-var (CId left-id (LocalId)))
                               (define left-r (rec-desugar left))]
                         (if (LexSlice? slice)
                             (local [(define slice-low (rec-desugar (LexSlice-lower slice)))
                                     (define slice-up (rec-desugar (LexSlice-upper slice)))
                                     (define slice-step (rec-desugar (LexSlice-step slice)))]
                               (CLet left-id
                                     (LocalId)
                                     left-r
                                     (CApp (py-getfield left-var
                                                      '__slice__)

                                           (list slice-low
                                                 slice-up slice-step)
                                           (none))))
                             (local [(define slice-r (rec-desugar slice))
                                     (define exn-id (new-id))] 
                               (CLet left-id
                                     (LocalId)
                                     left-r
                                     (CSeq
                                      (CTryExceptElse
                                       (py-getfield (CId left-id (LocalId))
                                                      '__getitem__)
                                       exn-id
                                       (default-except-handler
                                         exn-id
                                         (CRaise (some (make-exception 
                                                         'TypeError
                                                         "object is not subscriptable"))))
                                       (CNone))
                                      (CApp (py-getfield (CId left-id (LocalId))
                                                       '__getitem__)
                                            (list slice-r)
                                            (none) ;TODO: not sure what to do with stararg.
                                            ))))))]
                      [(symbol=? ctx 'Store)
                       (error 'desugar "bad syntax: LexSubscript has context 'Store' outside a LexAssign")]
                      [else (error 'desugar "unrecognized context in LexSubscript")])]
      
      [LexBreak () (CBreak)]

      [LexContinue () (CContinue)]

      [LexApp (fun args) (local [(define f (rec-desugar fun))
                                 (define f-expr f)
                                 (define results (map desugar args))]
                           (CApp f-expr results (none)))] 

      [LexAppStarArg (fun args sarg)
                     (local [(define f (rec-desugar fun))
                             (define results (map rec-desugar args))
                             (define s (rec-desugar sarg))]
                             (CApp f results (some s)))]
            
      [LexClass (scp name bases body)
                (make-class name
                            ;TODO: would be better to change bases to be a (listof LexExpr)
                            ;; and to build the tuple here (Alejandro).
                            ;; (CNone) is because we may not have a tuple class object yet.
                            (type-case CExpr (desugar bases)
                              [CTuple (class tuple) (CTuple (CNone) tuple)]
                              [else (error 'desugar "bases is not a tuple")])
                            (desugar body))]

      [LexInstanceId (x ctx)
                     (error 'desugar "should not encounter an instance ID!")]

      [LexDotField (value attr) (py-getfield (rec-desugar value) attr)]
      [LexExprField (value attr) (CGetAttr (rec-desugar value) (rec-desugar attr))]

      [LexTryExceptElse (try excepts orelse)
                        (local [(define try-r (rec-desugar try))
                                (define exn-id (new-id))
                                (define excepts-r (desugar-excepts exn-id excepts))
                                (define orelse-r (rec-desugar orelse))]
                          (CTryExceptElse 
                            try-r
                            exn-id
                            excepts-r
                            orelse-r))]

      [LexTryFinally (try finally)
                     (local [(define try-r (rec-desugar try))
                             (define finally-r (rec-desugar finally))]
                       (CTryFinally
                         try-r
                         finally-r))]

      [LexExcept (types body) (error 'desugar "should not encounter LexExcept!")]
      [LexExceptAs (types name body) (error 'desugar "should not encounter LexExcept!")]

      [LexWhile (test body orelse) (CWhile (rec-desugar test)
                                           (rec-desugar body)
                                           (rec-desugar orelse))]

      [LexFor (target iter body) (desugar-for target iter body)]

      ;; target is interpreted twice. FIX ME
      [LexAugAssign (op target value)
                    (local [(define target-r  target)
                            (define aug-r  (LexBinOp (context-load target) op value)) ]
                           (begin
                      (desugar (LexAssign (list target-r) (context-load aug-r)))))]
      ; XXX: target is interpreted twice, independently.
      ; Is there any case where this might cause problems?
      ; TODO: this whole thing needs re-writing.  I'm just converting it to do a standard assignment. 
      
      [LexDelete (targets)
                 (local
                  [(define (handle-delete target)
                     (type-case LexExpr target
                       [LexSubscript (left ctx slice)
                                     (letrec ([desugared-target (rec-desugar left)]
                                              [desugared-slice (rec-desugar slice)]
                                              [target-id (new-id)]
                                              [target-var (CId target-id (LocalId))])
                                       (CLet target-id (LocalId) desugared-target
                                             (CApp (py-getfield target-var
                                                              '__delitem__)
                                                   (list 
                                                    desugared-slice)
                                                   (none))))]
                       [LexLocalId (x ctx) (rec-desugar
                                            (LexAssign (list (LexLocalId x ctx)) (LexUndefined)))]
                       [LexGlobalId (x ctx) (rec-desugar
                                            (LexAssign (list (LexGlobalId x ctx)) (LexUndefined)))]
                       [else (error 'desugar (string-append "We don't know how to delete this yet: " (to-string target)))]))
                  (define (make-sequence [exprs : (listof CExpr)] )
                     (cond
                      [(empty? exprs) (error 'make-sequence "went too far")]
                      [(empty? (rest exprs)) (first exprs)]
                      [else (CSeq (first exprs) (make-sequence (rest exprs)))]))]
                  (make-sequence (map handle-delete targets)))]
      

      [LexImportFrom (module names asnames level) (rec-desugar (LexPass))]
                   ;(rec-desugar (desugar-importfrom-py module names asnames level) global? env opt-class)]       
      [LexBuiltinPrim (s args) (CBuiltinPrim s (map desugar args))]
      [else
        (error 'desugar
               (string-append
                 "deprecation warning: deprecated lexical construct reached desugar: "
                 (to-string expr)))]
      )))

(define (context-load e)
  (type-case LexExpr e
    [LexSubscript (left context slice) (LexSubscript left 'Load slice)]
    [else e]))

(define (desugar [expr : LexExpr]) : CExpr
  (rec-desugar expr))
