#lang plai-typed/untyped

(require "python-lexical-syntax.rkt")

(define (s-map f expr) : boolean (begin (map f expr) false))
(define (comma-separate [es : (listof LexExpr)]) : boolean
  (cond
   [(empty? es) false]
   [(empty? (rest es)) (begin (lexexpr-print (first es) "") false)]
   [else (begin (lexexpr-print (first es) "") (display ", ") (comma-separate (rest es)) false)]))

(define (comma-separate-2 [es : (listof symbol)]) : boolean
  (cond
   [(empty? es) false]
   [(empty? (rest es)) (begin (display (first es) ) false)]
   [else (begin (display (first es) ) (display ", ") (comma-separate-2 (rest es)) false)]))



(define (lexexpr-print [this-expr : LexExpr] [ starting-tab : string]) : LexExpr
  (let ((recur (lambda [y] [lexexpr-print y starting-tab])))
  (begin
    (type-case LexExpr this-expr
                                        ; control structures
      [LexIf (test body orelse)
             (begin
               (display starting-tab)
               (display "if ")
               (lexexpr-print test "")
               (display ":\n")
               (lexexpr-print body (string-append "  " starting-tab))
               (display "else:\n")
               (lexexpr-print orelse (string-append "  " starting-tab))
               (display "\n")
               this-expr)]
      [LexSeq (es) (begin (s-map recur es) this-expr)]
                                        ; the top level seq
      [LexModule (es) (begin
                        (display starting-tab)
                        (display "module:\n")
                        (s-map (lambda [y] [lexexpr-print y [string-append "  " starting-tab]]) es)this-expr)]
      [LexAssign (targets value)
                 (begin
                   (display starting-tab)
                   (comma-separate targets)
                   (display " = ")
                   (lexexpr-print value starting-tab )
                   (display "\n")
                   this-expr)]
      [LexAugAssign (op target value)
                    (begin
                      (display starting-tab)
                      (lexexpr-print target "")
                      (display op)
                      (display "=")
                      (lexexpr-print value "")
                      (display "\n")
                      this-expr)
                      ]
                                        ; primitive literals
      [LexNum (n) (begin (display starting-tab) (display n) this-expr)]
      [LexBool (n) (begin (display starting-tab) (display n) this-expr)]
      [LexBuiltinPrim (s args) (begin (display starting-tab) (display this-expr) this-expr)]
      [LexInstanceId (n ctx)  (begin (display starting-tab) (display n) (display " (instance) ") this-expr)]
      [LexGlobalId (n ctx) (begin (display starting-tab) (display n) (display " (global) ") this-expr)]
      [LexLocalId (n ctx) (begin (display starting-tab) (display n) (display " (local) ") this-expr)]
      [LexLocalLet (id bind body)
                   (begin
                     (display starting-tab)
                     (display "defvar ")
                     (display id)
                     (display " = ")
                     (lexexpr-print bind "")
                     (display " in {\n")
                     (lexexpr-print body (string-append "  " starting-tab))
                     (display starting-tab)
                     (display "}\n")
                     this-expr)
                     ]
      [LexInScopeLocals ( ls )
                        (begin
                          (display starting-tab)
                          (display "active locals: ")
                          (map display ls)
                          (display "\n")
                          this-expr)]
      [LexGlobals (ids body) (begin
                               (display starting-tab)
                               (display "defvars ")
                               (comma-separate-2 ids)
                               (display " = undefined in {\n")
                               (lexexpr-print body (string-append "  " starting-tab))
                               (display starting-tab)
                               (display "}\n")
                               this-expr)]
      [PyLexId (n ctx) (begin (display starting-tab) (display n) (display " (unknown scope) ")this-expr)]
      [PyLexGlobal (ids) (begin (display starting-tab) (display "global ") (comma-separate-2 ids ) this-expr)]
      [PyLexNonLocal (ids) (begin (display starting-tab) (display "nonlocal ") (comma-separate-2 ids)this-expr)]
      
                                        ; exceptions and exception handling
      [LexRaise (expr) (begin (display starting-tab) (display "raise ") (lexexpr-print expr "") (display "\n")this-expr) ]
      [LexExcept (types  body)
                 (begin
                   (display starting-tab)
                   (display "except ")
                   (comma-separate types)
                   (display ":\n")
                   (lexexpr-print body (string-append "  " starting-tab))
                   (display "\n")
                   this-expr)]
      [LexExceptAs (types name body)
                   (begin
                   (display starting-tab)
                   (display "except ")
                   (comma-separate types)
                   (display " as ")
                   (display name)
                   (display ":\n")
                   (lexexpr-print body (string-append "  " starting-tab))
                   (display "\n")
                   this-expr)]
      [LexTryExceptElse (try except orelse)
                        (begin
                        (display starting-tab)
                        (display "try:\n")
                        (lexexpr-print try (string-append "  " starting-tab))
                        (display "\n")
                        (map recur except)
                        (display "\n")
                        (display "else:")
                        (lexexpr-print orelse (string-append "  " starting-tab))
                        (display "\n")
                        this-expr)]
      [LexTryFinally (try finally)
                     (begin
                       (display starting-tab)
                       (display "try:\n")
                       (lexexpr-print try (string-append "  " starting-tab))
                       (display "\n")
                       (display "finally:")
                       (lexexpr-print finally (string-append "  " starting-tab))
                       (display "\n")
                       this-expr)]

      [LexYield (expr) (begin (display starting-tab) (display "yield ") (lexexpr-print expr "") this-expr)]
                                        ;loops 
      [LexWhile (test body orelse)
                (begin
                  (display starting-tab)
                  (display "while ")
                  (lexexpr-print test "")
                  (display ":\n")
                  (lexexpr-print body (string-append "  " starting-tab))
                  (display "\n")
                  (display starting-tab)
                  (display "else:\n")
                  (lexexpr-print orelse (string-append "  " starting-tab))
                  (display "\n")
                  this-expr)]
                  
      [LexFor (target iter body)
              (begin (display this-expr) this-expr)]

      
                                        ; pass & assert
      [LexPass () (begin
                    (display starting-tab)
                    (display "pass")
                    (display "\n")
                    this-expr)]
      [LexCore (_) (begin (display this-expr) this-expr)]
      [LexAssert (test msg)
                 (begin (display this-expr) this-expr)]
      
                                        ; classes and objects 
      [LexClass (scope name bases body)
                (begin
                  (display starting-tab)
                  (display "class ")
                  (display name)
                  (display ": extends ")
                  (display bases)
                  (lexexpr-print body (string-append "  " starting-tab))
                  this-expr)]
      [LexDotField (value attr)
                   (begin
                     (lexexpr-print value starting-tab)
                     (display ".")
                     (display attr )
                     this-expr)]
      [LexExprField (value attr)
                    (begin
                      (display this-expr)
                      this-expr)
                    ]
      
                                        ; operations
      [LexBinOp (left op right)
                (begin
                  (display starting-tab)
                  (lexexpr-print left "")
                  (display " ")
                  (display op)
                  (display " ")
                  (lexexpr-print right "")
                  this-expr
                  )] 
      [LexUnaryOp (op operand) (begin
                                 (display starting-tab)
                                 (display op)
                                 (lexexpr-print operand "")
                                 this-expr)]
      
      [LexCompOp (left ops comparators)
                 (begin
                   (display this-expr)
                   this-expr)]
      [LexBoolOp (op values)
                 (begin
                   (display this-expr)
                   this-expr)] ;op = 'And | 'Or
      
                                        ; functions
      [LexLam (args body)
              (begin
                (display starting-tab)
                (display "lambda ")
                (comma-separate-2 args)
                (display ": ")
                (lexexpr-print body (string-append " " starting-tab))
                (display "\n")
                this-expr)]
      [LexFunc (name args defaults body decorators class)
               (begin
                 (display starting-tab)
                 (if (empty? decorators)
                     (display "")
                     (display decorators))
                 (display "def ")
                 (display name)
                 (display "(")
                 (comma-separate-2 args)
                 (if (empty? defaults)
                     (display "):\n")
                     (begin
                       (display " (defaults: ")
                       (comma-separate defaults)
                       (display ")):\n")))
                 ;TAB
                 (lexexpr-print body (string-append "  " starting-tab))
                 (display "\n")
                 ;END TAB
                 this-expr)]
      [LexFuncVarArg (name args sarg body decorators class)
                     (begin
                       (display starting-tab)
                       (if (empty? decorators)
                           (display "")
                           (begin
                           (display decorators)
                           (display "\n")
                           (display starting-tab)))
                       (display "def ")
                       (display name)
                       (display "(")
                       (comma-separate-2 args)
                       (display ",")
                       (display sarg)
                       (display "*")
                       (display "):\n")
                                        ;TAB
                       (lexexpr-print body (string-append "  " starting-tab))
                       (display "\n")
                                        ;END TAB
                       this-expr)]
      [LexReturn (value) (begin
                           (display starting-tab)
                           (display "return ")
                           (lexexpr-print value "")
                           (display "\n")
                           this-expr)]
      [LexApp (fun args) (begin
                           (lexexpr-print fun starting-tab)
                           (display "(")
                           (comma-separate args)
                           (display ")")
                           this-expr)]
      [LexAppStarArg (fun args stararg)
                     (begin
                       (display this-expr)
                       this-expr)]
      
      [LexDelete (targets) (begin
                             (display starting-tab)
                             (display "delete ")
                             (comma-separate targets)
                             (display "\n")
                             this-expr)]
                                        ;
      [LexSubscript (left context slice)
                    (begin
                      (display starting-tab)
                      (display this-expr)
                      this-expr
                      )]
      
      [LexListComp (body generators)
                   (begin
                     (display this-expr)
                     this-expr)]
      [LexComprehen (target iter)
                    (begin 
                    (display this-expr)
                    this-expr)]
      
                                        ; builtin data structures
      [LexStr (s)  (begin (display starting-tab) (display s)this-expr)]
      [LexDict (keys values)
               (begin (display starting-tab)
                      (display "{ ")
                      (local
                       [(define (hlpr keys values)
                          (cond
                           [(empty? keys) (display "")]
                           [else (begin
                                   (display (first keys))
                                   (display " : ")
                                   (display (first values))
                                   (display ", ")
                                   (hlpr (rest keys) (rest values))
                                   )])
                          )] (hlpr keys values)
                       )
                      (display " }")
                      this-expr)]
      [LexList (values)
               (begin
                 (display starting-tab)
                 (display "[")
                 (comma-separate values)
                 (display "]")
                 this-expr)]
      [LexSlice (lower upper step)
                (begin
                  (display this-expr)
                  this-expr
                  )]
      [LexTuple (values) (begin
                 (display starting-tab)
                 (display "[")
                 (comma-separate values)
                 (display "]")
                 this-expr)]
      [LexUndefined [] (begin
                         (display starting-tab)
                         (display "UNDEF")
                         this-expr)]
      [LexSet (elts) (begin (display this-expr) this-expr)]
      [LexNone [] (begin
                    (display starting-tab)
                    (display "none")
                    this-expr)]
      [LexBreak [] (begin
                     (display starting-tab)
                     (display "break")
                     (display "\n")
                     this-expr)]
      [LexContinue [] (begin
                     (display starting-tab)
                     (display "continue")
                     (display "\n")
                     this-expr)]
      [LexBlock [a b] (begin
                        (display starting-tab)
                        (display "{\n")
                        (lexexpr-print b (string-append "  " starting-tab))
                        (display "\n")
                        (display starting-tab)
                        (display "}\n")
                        this-expr)]
      [LexImport (names asnames) (begin
                                   (display this-expr)
                                   this-expr)]
      [LexImportFrom (module names asnames level) (begin
                                                    (display this-expr)
                                                    this-expr)])
    this-expr
    )))
