#lang plai-typed

(require "python-lexical-syntax.rkt"
         "python-core-syntax.rkt"
         "util.rkt"
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
  (local [(define prelude
            (rec-desugar (LexPass))
            #;(if (not (empty? names))
                (rec-desugar (LexSeq (map (lambda (n) (LexAssign (list (LexId n 'Load))
                                                               (LexUndefined)))
                                         names))
                              g/ns-)
                (rec-desugar (LexPass)  g/ns-)))
          (define body (rec-desugar (LexSeq es)))]
     (CModule prelude body)))

(define (desugar-for [target : LexExpr] [iter : LexExpr]
                     [body : LexExpr]) : CExpr
  (local [(define iter-pyid (LexLocalId (new-id) 'Load))]
    (rec-desugar
     (LexSeq
      (list (LexAssign (list iter-pyid) (LexApp (LexGlobalId 'iter 'Load) (list iter)))
            (LexWhile (LexBool true)
                     (LexSeq 
                      (list 
                       (LexAssign (list target) (LexNone))
                       (LexTryExceptElseFinally
                         (LexAssign (list target) 
                                    (LexApp (LexDotField iter-pyid '__next__) empty))
                         (list (LexExcept (list (LexGlobalId 'StopIteration 'Load))
                                          (LexBreak)))
                         (LexPass)
                         (LexPass))
                       body))
                     (LexPass)))))))

(define (desugar-listcomp [body : LexExpr] [gens : (listof LexExpr)] ) : CExpr
  (local [(define list-id (LexLocalId (new-id) 'Load))
          (define (make-comploop gens)
            (cond 
              [(empty? gens) (LexApp (LexDotField list-id 'append) 
                                    (list body))]
              [(cons? gens)
               (LexFor (LexComprehen-target (first gens))
                      (LexComprehen-iter (first gens))
                      (make-comploop (rest gens)))]))
          (define full-expr
            (LexSeq
             (list 
              (LexAssign (list list-id) (LexList empty))
              (make-comploop gens)
              list-id)))]
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

#|  

;;; desugar import name as asname to
;;; asname = __import__('name')
(define (desugar-import-py names asnames) : PyExpr
  (local [(define (helper names asnames result)
            (cond [(empty? names) result]
                  [else
                   (helper (rest names) (rest asnames)
                           (append result
                                   (list (PyAssign (list (PyId (first asnames) 'Store))
                                                   (PyApp (PyId '__import__ 'Load)
                                                          (list (PyStr (first names))))))))]))]
         (PySeq (helper names asnames (list)))))

;;; desugar from module import name as asname
;;; __tmp_module = __import__(module, globals(), locals(), [id], 0)
;;; id_alias = __tmp_module.id
;;; 
;;;  from module import * will desugar to
;;; __import__all__(__import__(module, globals(), locals(), ['*'], 0)) 
;;; where __import__all__ performs importing based on __all__
(define (desugar-importfrom-py [module : string]
                               [names : (listof string)]
                               [asnames : (listof symbol)]
                               [level : number]) : PyExpr
  (local [(define tmp-module (PyId '__tmp_module 'Store))
          (define module-expr
            (PyAssign (list tmp-module)
                      (PyApp (PyId '__import__ 'Load)
                             (list (PyStr module)
                                   (PyApp (PyId '__globals 'Load) (list))
                                   (PyApp (PyId '__locals 'Load) (list))
                                   (PyList (map PyStr names)) ; list type of names
                                   (PyNum level)))))]
    (cond [(not (equal? (first names) "*"))
           (local [(define (get-bind-exprs module names asnames)
                     (cond [(empty? names) (list)]
                           [else
                            (append (list (PyAssign (list (PyId (first asnames) 'Store))
                                                    (PyDotField module (string->symbol (first names)))))
                                    (get-bind-exprs module (rest names) (rest asnames)))]))
                   (define bind-exprs
                     (get-bind-exprs tmp-module names asnames))]
             (PySeq (append (list module-expr) bind-exprs)))]
          [else ;; from module import *
           (PyApp (PyId '__import__all__ 'Load)
                  (list (PyApp (PyId '__import__ 'Load)
                               (list (PyStr module)
                                     (PyApp (PyId '__globals 'Load) (list))
                                     (PyApp (PyId '__locals 'Load) (list))
                                     (PyList (list (PyStr "*"))) ; list type of names
                                     (PyNum level)))))])))


|#

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
                                 (CApp (CGetField desugared-target '__setitem__)
                                        (list desugared-target
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
                                                         (CGetField (CId '$tuple_result (LocalId)) 
                                                                    '__getitem__)
                                                         (list (CId '$tuple_result (LocalId))
                                                               (make-builtin-num n))
                                                         (none))))
                                           targets-r
                                           (build-list (length targets-r) identity)))]
                              (CLet '$tuple_result (LocalId) value-r 
                                    (foldl (λ (a so-far) (CSeq so-far a))
                                           (first assigns) (rest assigns))))]
                  ; The others become a CAssign.
                  [else
                   (local [(define targets-r (map rec-desugar targets))
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
      [LexGlobalLet (x bind body) (CLet x (GlobalId) (rec-desugar bind) (rec-desugar body))]
      [LexLocalLet (x bind body) (CLet x (LocalId) (rec-desugar bind) (rec-desugar body))]
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
      
      ; LexPass is an empty lambda
      [LexPass () (CApp (CFunc empty (none) (CNone) false) empty (none))] 
      
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
                   ['Add (CApp (CGetField left-c '__add__) 
                               (list left-c right-c)
                               (none))]
                   ['Sub (CApp (CGetField left-c '__sub__) 
                               (list left-c right-c)
                               (none))]
                   ['Mult (CApp (CGetField left-c '__mult__)
                                (list left-c right-c)
                                (none))]
                   ['Div (CApp (CGetField left-c '__div__)
                               (list left-c right-c)
                               (none))]
                   ['FloorDiv (CApp (CGetField left-c '__floordiv__)
                                    (list left-c right-c)
                                    (none))]
                   ['Mod (CApp (CGetField left-c '__mod__)
                               (list left-c right-c)
                               (none))]
                   ['BitAnd (CApp (CGetField left-c '__and__)
                                  (list left-c right-c)
                                  (none))]
                   ['BitOr (CApp (CGetField left-c '__or__)
                                 (list left-c right-c)
                                 (none))]
                   ['BitXor (CApp (CGetField left-c '__xor__)
                                  (list left-c right-c)
                                  (none))]
                   ['Eq (CApp (CGetField left-c '__eq__)
                              (list left-c right-c)
                              (none))]
                   ['Gt (CApp (CGetField left-c '__gt__)
                              (list left-c right-c)
                              (none))]
                   ['Lt (CApp (CGetField left-c '__lt__)
                              (list left-c right-c)
                              (none))]
                   ['LtE (CApp (CGetField left-c '__lte__)
                               (list left-c right-c)
                               (none))]
                   ['GtE (CApp (CGetField left-c '__gte__)
                               (list left-c right-c)
                               (none))]
                   ['NotEq (rec-desugar (LexUnaryOp 'Not (LexBinOp left 'Eq right)))]
                   ['In (CApp (CFunc (list 'self 'test) (none)
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
                              (none))]
                   ['NotIn (rec-desugar (LexUnaryOp 'Not (LexBinOp left 'In right)))]
                   [else (CPrim2 op left-c right-c)]))]

      [LexUnaryOp (op operand)
                  (case op
                    ['USub (rec-desugar (LexBinOp (LexNum 0) 'Sub operand))]
                    ['UAdd (rec-desugar (LexBinOp (LexNum 0) 'Add operand))]
                    ['Invert (local [(define roperand (rec-desugar operand))]
                               (CApp (CGetField roperand '__invrt__)
                                     (list roperand)
                                     (none)))]
                    [else (CPrim1 op (rec-desugar operand))])]
      
      [LexBoolOp (op values) (desugar-boolop op values)]
      [LexCompOp (l op rights) (desugar-compop l op rights)]
      [LexListComp (elt gens) (desugar-listcomp elt gens)]
      [LexComprehen (target iter) (error 'desugar "Can't desugar LexComprehen")]
      
      [LexLam (args body) (CFunc args (none) (CReturn (rec-desugar body)) false)]

      [LexFunc (scp name args defargs body)
               (if (> (length defargs) 0)
                   (local [(define last-arg (first (reverse args)))]
                     (rec-desugar
                       ; assuming 1 defarg for now, generalize later
                       (LexSeq 
                         (list
                           (LexAssign (list (LexLocalId last-arg 'DesugarVar))
                                      (first (reverse defargs)))
                           (LexFuncVarArg scp name empty
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
                                              body)))))))
                   (local [(define body-r (rec-desugar body))]
                     (if (Instance-scoped? scp)
                         (error 'desugar "can't do instance functions yet")
                         (CAssign (CId name (which-scope scp))
                                  (CFunc args (none) body-r false)))))]
      
      ; a LexClassFunc is a method whose first argument should be the class rather than self
      [LexClassFunc (scp name args body)
                    (local [(define body-r (rec-desugar body))]
                      (if (Instance-scoped? scp)
                          (error 'desugar "can't do instance classfunctions yet")
                          (CAssign (CId name (which-scope scp))
                                   (CFunc args (none)
                                          ; We do this by, inside the function body,
                                          ; taking the first argument, which is "self",
                                          ; using that to look up the object's class, and then
                                          ; "overwriting" the first argument with that value.
                                          ; The result is that, in the function body, the first
                                          ; argument is the class, as expected.
                                          (CSeq (CAssign (CId (first args) (LocalId))
                                                         (CBuiltinPrim '$class
                                                                       (list (CId (first args)
                                                                                  (LocalId)))))
                                                body-r)
                                          false))))]
      
      [LexFuncVarArg (scp name args sarg body)
                     (if (Instance-scoped? scp)
                         (error 'desugar "can't do instance varargfunctions yet")
                         (CAssign (CId name (which-scope scp))
                                  (CFunc args (some sarg) (rec-desugar body) false)))]
      
      [LexReturn (value) (CReturn (rec-desugar value))]
      
      [LexDict (keys values) (CDict (lists->hash (map rec-desugar keys)
                                                 (map rec-desugar values)))]
      [LexSet (elts) (CSet (map rec-desugar elts))]
      [LexList (values) (CList (map rec-desugar values))]
      [LexTuple (values) (CTuple (map rec-desugar values))]
      
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
                                     (CApp (CGetField left-var
                                                      '__slice__)
                                           (list left-var slice-low
                                                 slice-up slice-step)
                                           (none))))
                             (local [(define slice-r (rec-desugar slice))] 
                               (CLet left-id
                                     (LocalId)
                                     left-r
                                     (CApp (CGetField left-var
                                                      '__getitem__)
                                           (list left-var slice-r)
                                           (none))))))]
                      [(symbol=? ctx 'Store)
                       (error 'desugar "bad syntax: LexSubscript has context 'Store' outside a LexAssign")]
                      [else (error 'desugar "unrecognized context in LexSubscript")])]
      
      [LexBreak () (CBreak)]
      
      ;; very hacky solution for assertRaises: it needs laziness built into it, so instead
      ;; of defining it as a function, we'll special case it as a macro.
      [LexApp (fun args)
              (cond
               [(or (and (LexLocalId? fun) (symbol=? (LexLocalId-x fun) '___assertRaises))
                    (and (LexGlobalId? fun) (symbol=? (LexGlobalId-x fun) '___assertRaises)))
                (local [(define f (rec-desugar (second args)))
                        (define as (map rec-desugar (rest (rest args))))
                        (define exns (rec-desugar (first args)))
                        (define pass (rec-desugar (LexPass)))]
                  (CApp
                    (CFunc empty (none)
                           (CTryExceptElseFinally
                             (CApp f as (none))
                             (list
                               (CExcept (list exns) (none) pass)
                               (CExcept empty (none) pass))
                             (CApp (CId 'print (GlobalId))
                                   (list (make-builtin-str "Assert failure!"))
                                   (none))
                             pass)
                           false)
                    empty
                    (none)))]
               [(or (and (LexLocalId? fun) (symbol=? (LexLocalId-x fun) 'locals))
                    (and (LexGlobalId? fun) (symbol=? (LexGlobalId-x fun) 'locals)))
                (CBuiltinPrim '$locals empty)]
               [else
                 (local [(define f (rec-desugar fun))
                         (define f-expr f)
                         (define results (map rec-desugar args))]
                   (cond
                     [(CGetField? f-expr)
                      ;; FIX THIS
                      (local [(define o (CGetField-value f-expr))]
                        (CApp f-expr (cons o results) (none)))]
                     ; special case: "super" application gets extra 'self' argument
                     [(and (CId? f-expr) (symbol=? 'super (CId-x f-expr)))
                      (CApp f-expr (cons (CId 'self (LocalId)) results) (none))]
                     [else (CApp f-expr results (none))]))])]
      
      [LexAppStarArg (fun args sarg)
                     (local [(define f (rec-desugar fun))
                             (define results (map rec-desugar args))
                             (define s (rec-desugar sarg))]
                       (if (CGetField? f)
                           (local [(define o (CGetField-value f))]
                             (CApp f (cons o results) (some s)))
                           (CApp f results (some s))))]
      
      [LexClass (scp name bases body)
                (local [(define (get-names expr)
                          (lexexpr-fold-tree expr
                                             (lambda (y)
                                               (type-case LexExpr y
                                                 [LexInstanceId (x ctx) (list x)]
                                                 [LexClass (scp name bases body) empty]
                                                 [LexBlock (_ __) empty]
                                                 [else (haiku-error)]))))
                        (define (replace-instace expr)
                          (lexexpr-modify-tree
                            expr
                            (lambda (y)
                              (type-case LexExpr y
                                [LexInstanceId (x ctx)
                                               (LexDotField
                                                 (cond
                                                   [(Globally-scoped? scp) (LexGlobalId name 'Load)]
                                                   [(Locally-scoped? scp) (LexLocalId name 'Load)]
                                                   [(Instance-scoped? scp) (LexInstanceId name 'Load)]
                                                   [else (error 'desugar "could not determine scoping for parent class")])
                                                 x)]
                                [else (haiku-error)]))))
                        (define names (get-names body))
                        (define body-r (desugar (replace-instace body)))
                        (define modbody
                          (if (member '__init__ names)
                              body-r
                              (CSeq body-r
                                    (CAssign 
                                      (CId '__init__ (LocalId))
                                      (CFunc (list 'self) (none)
                                             (CAssign 
                                               (CGetField
                                                 (CId 'self (LocalId))
                                                 '__class__)
                                               (CBuiltinPrim '$class
                                                             (list (CId 'self (LocalId)))))
                                             true)))))]                       
                  (CAssign (CId name (which-scope scp))
                           (CClass name
                                   (if (empty? bases)
                                       (list 'object)
                                       bases)
                                   modbody)))]

      [LexInstanceId (x ctx)
                     (error 'desugar "should not encounter an instance ID outside of a class!")]

      [LexDotField (value attr) (CGetField (rec-desugar value) attr)]
      
      [LexTryExceptElseFinally (try excepts orelse finally)
                               (local [(define try-r (rec-desugar try))
                                       (define excepts-r (map rec-desugar excepts))
                                       (define orelse-r (rec-desugar orelse))
                                       (define finally-r (rec-desugar finally))]
                                 (CTryExceptElseFinally 
                                   try-r
                                   excepts-r
                                   orelse-r
                                   finally-r))]
      
      [LexExcept (types body) (CExcept (map rec-desugar types)
                                       (none)
                                       (rec-desugar body))]
      
      [LexWhile (test body orelse) (CWhile (rec-desugar test)
                                           (rec-desugar body)
                                           (rec-desugar orelse))]
      [LexFor (target iter body) (desugar-for target iter body)]
      
      [LexExceptAs (types name body)
                       (CExcept (map rec-desugar types) 
                                (some name)
                                (rec-desugar body))]
     
      ;; target is interpreted twice. FIX ME
      [LexAugAssign (op target value)
                    (local [(define target-r (rec-desugar target))
                            (define aug-r (rec-desugar (LexBinOp target op value)))]
                      (CAssign target-r aug-r))]
      ; XXX: target is interpreted twice, independently.
      ; Is there any case where this might cause problems?
      
      [LexDelete (targets)
                 (let ([target (first targets)]) ; TODO: handle deletion of more than one target
                   (type-case LexExpr target
                     [LexSubscript (left ctx slice)
                                   (letrec ([desugared-target (rec-desugar left)]
                                            [desugared-slice (rec-desugar slice)]
                                            [target-id (new-id)]
                                            [target-var (CId target-id (LocalId))])
                                     (CLet target-id (LocalId) desugared-target
                                           (CApp (CGetField target-var
                                                            '__delitem__)
                                                 (list target-var
                                                       desugared-slice)
                                                 (none))))]
                     [else (error 'desugar "We don't know how to delete identifiers yet.")]))]
     
      [LexImport (names asnames) (rec-desugar (LexPass))]
                 ;(rec-desugar (desugar-import-py names asnames))]

      [LexImportFrom (module names asnames level) (rec-desugar (LexPass))]
                     ;(rec-desugar (desugar-importfrom-py module names asnames level))]


      [else
        (error 'desugar
               (string-append
                 "deprecation warning: deprecated lexical construct reached desugar: "
                 (to-string expr)))]

      )))

(define (desugar [expr : LexExpr]) : CExpr
  (rec-desugar expr))
