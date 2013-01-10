#lang plai-typed

(require "python-lexical-syntax.rkt"
         "python-core-syntax.rkt"
         "util.rkt"
         "builtins/num.rkt"
         "python-syntax-operations.rkt"
         "builtins/str.rkt")
(require (typed-in racket/base (number->string : (number -> string)))
         (typed-in racket/base (append : ((listof 'a) (listof 'a) -> (listof 'a))))
         (typed-in racket/base (cdr : (('a * 'b)  -> 'b)))
         (typed-in racket/base (car : (('a * 'b)  -> 'a)))
         (typed-in racket/list (last : ((listof 'a) -> 'a)))
         (typed-in racket/list (count : (('a -> boolean) (listof 'a) -> number)))
         (typed-in racket/list (take : ((listof 'a) number -> (listof 'a))))
         (typed-in racket/base (append : ((listof 'a) (listof 'a) -> (listof 'a)))))


(define (desugar-boolop [op : symbol] 
                        [values : (listof LexExpr)]
                        ) : CExpr
  (begin ;(display "binop: ") (display values) (display "\n\n")
    (local [(define first-val (rec-desugar (first values)  ))]
      (if (> (length values) 1)
          (local [(define rest-val
                    (desugar-boolop op (rest values) ))]
            (case op
              ['And 
                     (CIf (identity first-val)
                          (identity rest-val) 
                          (identity first-val))]
              ['Or 
                    (CIf (identity first-val)
                         (identity first-val)
                         (identity rest-val))
                    ]))
          first-val))))

(define (desugar-compop [l : LexExpr] 
                        [ops : (listof symbol)] 
                        [comparators : (listof LexExpr)]
                        ) : CExpr
  (begin ;(display "compop: ") (display comparators) (display "\n")
    ;(display ops) (display "\n")
    ;(display l) (display "\n")
    (local [;(define first-right (rec-desugar (first comparators)  ))
            ;(define l-expr (rec-desugar l  (identity- first-right)))
            (define first-comp (rec-desugar (LexBinOp l (first ops) (first
                                                                    comparators))
                                             ))]
      (if (> (length comparators) 1) 
          (local [(define rest-comp (desugar-compop (first comparators)
                                                    (rest ops)
                                                    (rest comparators)
                                                    
                                                    ))]

                 (CIf (identity first-comp)
                      (identity rest-comp)
                      (identity first-comp)))
          
          first-comp))))



(define (desugar-pymodule [es : (listof LexExpr)] 
                          ) : CExpr
  (local [;(define g/ns- (get-globals/nonlocals (LexSeq es)  ))
          ;(define names  (get-names (LexSeq es)  g/ns-))
          (define prelude
            (rec-desugar (LexPass) )
            #;(if (not (empty? names))
                (rec-desugar (LexSeq (map (lambda (n) (LexAssign (list (LexId n 'Load))
                                                               (LexUndefined)))
                                         names))
                              g/ns-)
                (rec-desugar (LexPass)  g/ns-)))
          (define body (rec-desugar (LexSeq es)  ))]
     (CModule
      (identity prelude)
      (identity body))
     ))

#;(define (map-desugar [exprs : (listof LexExpr)]
                      ): ((listof CExpr) * Id)
  (local [(define (rec-map-desugar exps g e)
            (cond
              [(empty? exps) (values empty e)]
              [(cons? exps)
               (local [(define first-r (rec-desugar (first exps) e))
                       (define-values (results last-)
                         (rec-map-desugar (rest exps) ))]
                 (values
                  (cons (identity first-r)
                        results)
                  last-))]))]
    (rec-map-desugar exprs  )))


(define (desugar-for [target : LexExpr] [iter : LexExpr] [body : LexExpr]
                      ) : CExpr
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
                     (LexPass))))
     
     
    )))

(define (desugar-listcomp [body : LexExpr] [gens : (listof LexExpr)] 
                           ) : CExpr
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
    (rec-desugar full-expr )))

(define (which-scope [scp : LocalOrGlobal]) : IdType
  (type-case LocalOrGlobal scp
    [Locally-scoped () (LocalId)]
    [Globally-scoped () (GlobalId)]
    [Unknown-scope () (error 'desugar: "somehow an Unknown-scope got past the lexical stage.")]
    [Instance-scoped () (error 'desugar: "this is an instance variable, please handle it accordingly")]))

(define (desugar-seq [es : (listof LexExpr)]) : CExpr
  (cond
   [(empty? es) (error 'desugar "empty sequences are not supported")]
   [(empty? (rest es)) (desugar (first es))]
   [else (CSeq (desugar (first es)) (desugar-seq (rest es)))]))
      

(define (rec-desugar [expr : LexExpr] ) : CExpr 
  (begin ;(display expr) (display "\n\n")
    (type-case LexExpr expr
      [LexSeq (es) (desugar-seq es)]
      [LexModule (es) (desugar-pymodule es  )]
      [LexAssign (targets value) 
                (type-case LexExpr (first targets) 
                  ; We handle three kinds of assignments.
                  ; An assignment to a subscript is desugared as a __setitem__ call.
                  [LexSubscript (left ctx slice)
                               (letrec ([desugared-target (rec-desugar left )]
                                        [desugared-slice 
                                         (rec-desugar slice)]
                                        [desugared-value
                                         (rec-desugar value)]
                                        [target-id (new-id)])
                                 (CApp (CGetField (identity desugared-target) '__setitem__)
                                        (list (identity desugared-target)
                                              (identity desugared-slice)
                                              (identity desugared-value))
                                        (none))
                                 )]
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
                              (CLet '$tuple_result (LocalId)  (identity value-r) 
                                    (foldl (λ (a so-far) (CSeq so-far a))
                                           (first assigns) (rest assigns)))
                              )]
                  
                  ; The others become a CAssign.
                  [else
                   (local [(define targets-r
                             (map rec-desugar targets  ))
                           (define value-r (rec-desugar value))]
                     
                          (foldl (lambda (t so-far)
                                   (CSeq so-far (CAssign t (identity value-r))))
                                 (CAssign (first targets-r) (identity value-r))
                                 (rest targets-r))
                          )])]
      
      [LexNum (n) (identity (make-builtin-num n) )]
      [LexSlice (lower upper step) (error 'desugar "Shouldn't desugar slice directly")]
      [LexBool (b) (identity (if b (CTrue) (CFalse)) )]
      [LexNone () (identity (CNone) )]
      [LexStr (s) (identity (make-builtin-str s) )]
      [LexLocalId (x ctx)  (CId x (LocalId)) ]
      [LexGlobalId (x ctx)  (CId x (GlobalId)) ]
      [LexGlobalLet (x bind body) (CLet x (GlobalId) (rec-desugar bind) (rec-desugar body))]
      [LexLocalLet (x bind body) (CLet x (LocalId) (rec-desugar bind) (rec-desugar body))]
      [LexUndefined () (identity (CUndefined) )]  
      
      [LexRaise (expr) (local [(define expr-r
                                 (if
                                  (or (LexLocalId? expr) (LexGlobalId? expr))
                                  ;;handle the implicit construction case
                                  (rec-desugar (LexApp expr empty)
                                               ) 
                                  (rec-desugar expr  )))]
                         (CRaise 
                          (if (LexPass? expr)
                              (none)
                              (some (identity expr-r))))
                         )]
      
      ; LexPass is an empty lambda
      [LexPass () (identity (CApp (CFunc empty (none) (CNone) false) empty (none)) )] 
      
      [LexIf (test body orelse)
            (local [(define test-r (rec-desugar test))
                    (define body-r (rec-desugar body))
                    (define orelse-r (rec-desugar orelse))]
               (CIf (identity test-r)
                    (identity body-r)
                    (identity orelse-r))
               )]
      
      [LexBinOp (left op right)
               (local [(define left-r (rec-desugar left  ))
                       (define left-c (identity left-r))
                       (define right-r (rec-desugar right))
                       (define right-c (identity right-r))] 
                 (case op 
                   ['Add (identity (CApp (CGetField left-c '__add__) 
                                        (list left-c right-c)
                                        (none))
                                  )]
                   ['Sub (identity (CApp (CGetField left-c '__sub__) 
                                        (list left-c right-c)
                                        (none))
                                  )]
                   ['Mult (identity (CApp (CGetField left-c '__mult__)
                                         (list left-c right-c)
                                         (none))
                                   )]
                   ['Div (identity (CApp (CGetField left-c '__div__)
                                        (list left-c right-c)
                                        (none))
                                  )]
                   ['FloorDiv (identity (CApp (CGetField left-c '__floordiv__)
                                             (list left-c right-c)
                                             (none))
                                       )]
                   ['Mod (identity (CApp (CGetField left-c '__mod__)
                                        (list left-c right-c)
                                        (none))
                                  )]
                   ['BitAnd (identity (CApp (CGetField left-c '__and__)
                                           (list left-c right-c)
                                           (none))
                                     )]
                   ['BitOr (identity (CApp (CGetField left-c '__or__)
                                          (list left-c right-c)
                                          (none))
                                    )]
                   ['BitXor (identity (CApp (CGetField left-c '__xor__)
                                           (list left-c right-c)
                                           (none))
                                     )]
                   ['Eq (identity (CApp (CGetField left-c '__eq__)
                                       (list left-c right-c)
                                       (none))
                                 )]
                   ['Gt (identity (CApp (CGetField left-c '__gt__)
                                       (list left-c right-c)
                                       (none))
                                 )]
                   ['Lt (identity (CApp (CGetField left-c '__lt__)
                                       (list left-c right-c)
                                       (none))
                                 )]
                   ['LtE (identity (CApp (CGetField left-c '__lte__)
                                        (list left-c right-c)
                                        (none))
                                  )]
                   ['GtE (identity (CApp (CGetField left-c '__gte__)
                                        (list left-c right-c)
                                        (none))
                                  )]
                   ['NotEq (rec-desugar (LexUnaryOp 'Not (LexBinOp left 'Eq right)) 
                                         )]
                   ['In (identity 
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
                         )]
                   ['NotIn (rec-desugar (LexUnaryOp 'Not (LexBinOp left 'In right))
                                         )]
                   [else (identity
                          (CPrim2 op left-c right-c)
                          )]))]
      
      [LexUnaryOp (op operand)
                 (case op
                   ['USub (rec-desugar (LexBinOp (LexNum 0) 'Sub operand)  )]
                   ['UAdd (rec-desugar (LexBinOp (LexNum 0) 'Add operand)  )]
                   ['Invert (local [(define roperand (rec-desugar operand  ))]
                              (identity 
                               (CApp (CGetField (identity roperand) '__invrt__)
                                     (list (identity roperand))
                                     (none))
                               ))]
                   [else (local [(define roperand (rec-desugar operand  ))]
                           (identity (CPrim1 op 
                                            (identity roperand)) 
                                    ))])]
      
      [LexBoolOp (op values) (desugar-boolop op values  )]
      
      [LexCompOp (l op rights) (local [(define c (desugar-compop l op rights  ))]
                                (begin ;(display c) (display "\n")
                                  c))]
      
      [LexListComp (elt gens) (desugar-listcomp elt gens  )]
      [LexComprehen (target iter) (error 'desugar "Can't desugar LexComprehen")]
      
      
      [LexLam (args body)
             (local [(define rbody (rec-desugar body  ))]
               (identity 
                (CFunc args (none)
                       (CReturn                   
                        (identity rbody))
                       false
                      )
                ))]
      
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
                                                        (LexDotField (LexLocalId 'stararg 'Load)
                                                                    '__len__)
                                                        empty)
                                                       (list 'Gt)
                                                       (list (LexNum 0)))
                                             (LexAssign (list (LexLocalId last-arg
                                                                   'DesugarVar))
                                                       (LexSubscript (LexLocalId 'stararg 'Load)
                                                                    'Load
                                                                    (LexNum 0)))
                                             (LexPass))
                                       body))))) 
                      
                     
                    ))
                  
                  (local [(define body-r (rec-desugar body))]
                         (if (Instance-scoped? scp)
                             (error 'desugar "can't do instance functions yet")
                             (CAssign (CId name (which-scope scp))
                                      (CFunc args (none) body-r false)))
                     ))]
      
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
                                                                      (list (CId
                                                                             (first
                                                                              args)
                                                                             (LocalId)))))
                                               (identity body-r))
                                         false))
                         ))]
      
      [LexFuncVarArg (scp name args sarg body)
                    (local [(define body-r 
                              (rec-desugar body ))]
                      (if (Instance-scoped? scp)
                          (error 'desugar "can't do instance varargfunctions yet")
                          (CAssign (CId name (which-scope scp))
                                   (CFunc args (some sarg) (identity body-r) false))
                          ))]
      
      [LexReturn (value)
                (local [(define value-r (rec-desugar value  ))]
                       (CReturn value-r))]
      
      [LexDict (keys values) 
              (local [(define keys-r (map rec-desugar keys))
                      (define values-r
                        (map desugar values))]
                (identity (CDict (lists->hash keys-r values-r))))]
      
      [LexSet (elts) (local [(define results
                              (map desugar elts  ))]
                      (identity (CSet results) ))]
      
      [LexList (values) (local [(define results
                                 (map desugar values))]
                         (identity (CList results) ))]
      
      [LexTuple (values) (local [(define results
                                  (map desugar values  ))]
                          (identity (CTuple results) ))]
      
      [LexSubscript (left ctx slice)
                   (cond
                     [(symbol=? ctx 'Load)
                      (local [(define left-id (new-id))
                              (define left-var (CId left-id (LocalId)))
                              (define left-r (rec-desugar left  ))]
                        (if (LexSlice? slice)
                            (local [(define slice-low (rec-desugar (LexSlice-lower slice)
                                                                    ))
                                    (define slice-up (rec-desugar (LexSlice-upper slice)
                                                                   ))
                                    (define slice-step (rec-desugar (LexSlice-step slice)
                                                                     ))]
                              (identity
                               (CLet left-id
                                     (LocalId)
                                     (identity left-r)
                                     (CApp (CGetField left-var
                                                      '__slice__)
                                           (list 
                                            left-var
                                            (identity slice-low)
                                            (identity slice-up)
                                            (identity slice-step))
                                           (none)))
                               ))
                            (local [(define slice-r (rec-desugar slice 
                                                                 ))] 
                              (identity 
                               (CLet left-id
                                     (LocalId)
                                     (identity left-r)
                                     (CApp (CGetField left-var
                                                      '__getitem__)
                                           (list left-var
                                                 (identity slice-r))
                                           (none)))
                               ))))]
                     [(symbol=? ctx 'Store)
                      (error 'desugar "bad syntax: LexSubscript has context 'Store' outside a LexAssign")]
                     [else (error 'desugar "unrecognized context in LexSubscript")])]
      
      [LexBreak () (identity (CBreak) )]
      
      ;; very hacky solution for assertRaises: it needs laziness built into it, so instead
      ;; of defining it as a function, we'll special case it as a macro.
      [LexApp (fun args)
              (cond
               [(or (and (LexLocalId? fun) (symbol=? (LexLocalId-x fun) '___assertRaises))
                    (and (LexGlobalId? fun) (symbol=? (LexGlobalId-x fun) '___assertRaises)))
                (local [(define f (rec-desugar (second args)  ))
                        (define as
                          (map desugar (rest (rest args))  ))
                        (define exns (rec-desugar (first args)))
                        (define pass (rec-desugar (LexPass)))]
                  (identity
                   (CApp
                    (CFunc empty (none)
                           (CTryExceptElseFinally
                            (CApp (identity f) as (none))
                            (list
                             (CExcept (list (identity exns)) (none) (identity pass))
                             (CExcept empty (none) (identity pass)))
                            (CApp (CId 'print (GlobalId))
                                  (list (make-builtin-str "Assert failure!"))
                                  (none))
                            (identity pass))
                          false)
                    empty
                    (none))
                   ))]
               [(or (and (LexLocalId? fun) (symbol=? (LexLocalId-x fun) 'locals))
                    (and (LexGlobalId? fun) (symbol=? (LexGlobalId-x fun) 'locals)))
                (identity
                 (CBuiltinPrim '$locals empty)
                 )]
               [else
                (local [(define f (rec-desugar fun  ))
                        (define f-expr (identity f))
                        (define results
                          (map desugar args  ))]
                  (identity
                   (cond
                     [(CGetField? f-expr)
                      (local [(define o (CGetField-value f-expr))]
                        (CApp f-expr (cons o results) (none)))]
                     ; special case: "super" application gets extra 'self' argument
                     [(and (CId? f-expr) (symbol=? 'super (CId-x f-expr)))
                      (CApp f-expr (cons (CId 'self (LocalId)) results) (none))]
                     [else (CApp f-expr results (none))])
                   ))])]
      
      [LexAppStarArg (fun args sarg)
                    (local [(define f (rec-desugar fun  ))
                            (define results  
                              (map desugar args  ))
                            (define s (rec-desugar sarg))]
                      (identity
                       (if (CGetField? (identity f))
                           (local [(define o (CGetField-value (identity f)))]
                             (CApp (identity f) (cons o results) (some (identity s))))
                           (CApp (identity f) results (some (identity s))))
                       ))]
      
      [LexClass (scp name bases body)
               (local [
                       (define (get-names expr)
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
                              [LexInstanceId (x ctx) (LexDotField
                                                      (cond
                                                       [(Globally-scoped? scp) (LexGlobalId name 'Load) ]
                                                       [(Locally-scoped? scp) (LexLocalId name 'Load)]
                                                       [(Instance-scoped? scp) (LexInstanceId name 'Load)]
                                                       [else (error 'desugar "could not determine scoping for parent class")])
                                                      x)]
                              [else (haiku-error)]))))
                       (define names (get-names body))
                       (define body-r (desugar (replace-instace body)))
                       (define modbody
                         (if (member '__init__ names)
                             (identity body-r)
                             (CSeq (identity body-r)
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
                 (identity
                  (CAssign (CId name (which-scope scp))
                           (CClass name
                                   (if (empty? bases)
                                       'object
                                       (first bases))
                                   modbody))
                  ))]
      [LexInstanceId (x ctx) (error 'desugar "should not encounter an instance ID outside of a class!")]
      [LexDotField (value attr)
                  (local [(define value-r (rec-desugar value  ))]
                    (identity
                     (CGetField (identity value-r) attr)
                     ))]
      
      [LexTryExceptElseFinally (try excepts orelse finally)
                              (local [(define try-r (rec-desugar try  ))
                                      (define excepts-r
                                        (map desugar excepts  ))
                                      (define orelse-r (rec-desugar orelse))
                                      (define finally-r (rec-desugar finally 
                                                                     ))]
                                (identity
                                 (CTryExceptElseFinally 
                                  (identity try-r)
                                  excepts-r
                                  (identity orelse-r)
                                  (identity finally-r))
                                 ))]
      
      [LexExcept (types body)
                (local [(define types-r 
                          (map desugar types  ))
                        (define body-r (rec-desugar body  ))]
                  (identity
                   (CExcept types-r 
                            (none)
                            (identity body-r))
                   ))]
      
      [LexWhile (test body orelse)
               (local [(define test-r (rec-desugar test  ))
                       (define body-r (rec-desugar body  ))
                       (define orelse-r (rec-desugar orelse  ))]
                 (identity 
                  (CWhile (identity test-r)
                          (identity body-r)
                          (identity orelse-r))
                  ))]
      [LexFor (target iter body) (desugar-for target iter body  )]
      
      [LexExceptAs (types name body)
                  (local [(define types-r
                            (map desugar types  ))
                          (define body-r (rec-desugar body))]
                    (identity
                     (CExcept types-r
                              (some name)
                              (identity body-r))
                      ))]
      
      [LexAugAssign (op target value)
                   (local [(define target-r (rec-desugar target  ))
                           (define aug-r (rec-desugar (LexBinOp target op value)
                                                       ))]
                     (identity
                      (CAssign (identity target-r)
                               (identity aug-r))
                      ))]
      ; XXX: target is interpreted twice, independently.
      ; Is there any case where this might cause problems?
      
      [LexDelete (targets)
                (let ([target (first targets)]) ; TODO: handle deletion of more than one target
                  (type-case LexExpr target
                    [LexSubscript (left ctx slice)
                                 (letrec ([desugared-target (rec-desugar left  )]
                                          [desugared-slice
                                           (rec-desugar slice  )]
                                          [target-id (new-id)]
                                          [target-var (CId target-id (LocalId))])
                                   (identity
                                    (CLet target-id (LocalId) (identity desugared-target)
                                          (CApp (CGetField target-var
                                                           '__delitem__)
                                                (list target-var
                                                      (identity desugared-slice))
                                                (none)))
                                    ))]
                    [else (error 'desugar "We don't know how to delete identifiers yet.")]))]
      [else (error 'desugar (string-append "deprecation warning: deprecated lexical construct reached desugar: " (to-string expr)))]
      )))

(define (desugar [expr : LexExpr]) : CExpr
  (rec-desugar expr))
