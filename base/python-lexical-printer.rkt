#lang plai-typed/untyped

(require "python-lexical-syntax.rkt")

(define (s-map f expr) (begin (map f expr) (first expr)))
(define (comma-separate es)
  (cond
   [(empty? es) false]
   [(empty? (rest es)) (begin (lexexpr-print (first es) "") false)]
   [else (begin (lexexpr-print (first es) "") (display ", ") (comma-separate (rest es)) false)]))

(define (comma-separate-2 es)
  (cond
   [(empty? es) false]
   [(empty? (rest es)) (begin (display (first es) ) false)]
   [else (begin (display (first es) ) (display ", ") (comma-separate (rest es)) false)]))



(define (lexexpr-print this-expr starting-tab)
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
               (display "\n"))]
      [LexSeq (es) (s-map recur es)]
                                        ; the top level seq
      [LexModule (es) (begin
                        (display starting-tab)
                        (display "module:\n")
                        (s-map (lambda [y] [lexexpr-print y [string-append "  " starting-tab]]) es))]
      [LexAssign (targets value)
                 (begin
                   (display starting-tab)
                   (comma-separate targets)
                   (display " = ")
                   (lexexpr-print value starting-tab )
                   (display "\n"))]
      [LexAugAssign (op target value)
                    (begin
                      (display starting-tab)
                      (lexexpr-print target "")
                      (display op)
                      (display "=")
                      (lexexpr-print value "")
                      (display "\n"))
                      ]
                                        ; primitive literals
      [LexNum (n) (begin (display starting-tab) (display n))]
      [LexBool (n) (begin (display starting-tab) (display n))]
      [LexBuiltinPrim (s args) (begin (display starting-tab) (display this-expr))]
      [LexInstanceId (n ctx)  (begin (display starting-tab) (display n) (display " (instance) "))]
      [LexGlobalId (n ctx) (begin (display starting-tab) (display n) (display " (global) "))]
      [LexLocalId (n ctx) (begin (display starting-tab) (display n) (display " (local) "))]
      [LexLocalLet (id bind body)
                   (begin
                     (display starting-tab)
                     (display "defvar ")
                     (display id)
                     (display " = ")
                     (lexexpr-print bind)
                     (display " in {\n")
                     (lexexpr-print body (string-append "  " starting-tab))
                     (display starting-tab)
                     (display "}\n"))
                     ]
      [LexInScopeLocals ( ls )
                        (begin
                          (display starting-tab)
                          (display "active locals: ")
                          (map display ls)
                          (display "\n"))]
      [LexGlobals (ids body) (begin
                               (display starting-tab)
                               (display "defvars ")
                               (comma-separate-2 ids)
                               (display " = undefined in {\n")
                               (lexexpr-print body (string-append "  " starting-tab))
                               (display starting-tab)
                               (display "}\n"))]
      [PyLexId (n ctx) (begin (display starting-tab) (display n) (display " (unknown scope) "))]
      [PyLexGlobal (ids) (begin (display starting-tab) (display "global ") (comma-separate-2 ids ) )]
      [PyLexNonLocal (ids) (begin (display starting-tab) (display "nonlocal ") (comma-separate-2 ids))]
      
                                        ; exceptions and exception handling
      [LexRaise (expr) (begin (display starting-tab) (display "raise ") (lexexpr-print expr "") (display "\n")) ]
      [LexExcept (types  body)
                 (begin
                   (display starting-tab)
                   (display "except ")
                   (comma-separate types)
                   (display ":\n")
                   (lexexpr-print body (string-append "  " starting-tab))
                   (display "\n"))]
      [LexExceptAs (types name body)
                   (begin
                   (display starting-tab)
                   (display "except ")
                   (comma-separate types)
                   (display " as ")
                   (display name)
                   (display ":\n")
                   (lexexpr-print body (string-append "  " starting-tab))
                   (display "\n"))]
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
                        (display "\n"))]
      [LexTryFinally (try finally)
                     (begin
                       (display starting-tab)
                       (display "try:\n")
                       (lexexpr-print try (string-append "  " starting-tab))
                       (display "\n")
                       (display "finally:")
                       (lexexpr-print finally (string-append "  " starting-tab))
                       (display "\n"))]

      [LexYield (expr) (begin (display starting-tab) (display "yield ") (lexexpr-print expr "") )]
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
                  (display "\n"))]
                  
      [LexFor (target iter body)
              (display this-expr)]

      
                                        ; pass & assert
      [LexPass () (begin
                    (display starting-tab)
                    (display "pass")
                    (display "\n"))]
      [LexCore (_) (display this-expr)]
      [LexAssert (test msg)
                 (display this-expr)]
      
                                        ; classes and objects 
      [LexClass (scope name bases body)
                (begin
                  (display starting-tab)
                  (display "class ")
                  (display name)
                  (display ": extends ")
                  (display bases)
                  (lexexpr-print body (string-append "  " starting-tab)))]
      [LexDotField (value attr)
                   (begin
                     (lexexpr-print value starting-tab)
                     (display ".")
                     (lexexpr-print attr ""))]
      [LexExprField (value attr)
                    (display this-expr)
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
                  )] 
      [LexUnaryOp (op operand) (begin
                                 (display starting-tab)
                                 (display op)
                                 (lexexpr-print operand ""))]
      
      [LexCompOp (left ops comparators)
                 (display this-expr)]
      [LexBoolOp (op values)
                 (display this-expr)] ;op = 'And | 'Or
      
                                        ; functions
      [LexLam (args body)
              (begin
                (display starting-tab)
                (display "lambda ")
                (comma-separate-2 args)
                (display ": ")
                (lexexpr-print body (string-append " " starting-tab))
                (display "\n"))]
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
                 )]
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
                       )]
      [LexReturn (value) (begin
                           (display starting-tab)
                           (display "return ")
                           (lexexpr-print value "")
                           (display "\n"))]
      [LexApp (fun args) (begin
                           (lexexpr-print fun starting-tab)
                           (display "(")
                           (comma-separate args)
                           (display ")"))]
      [LexAppStarArg (fun args stararg)
                     (display this-expr)]
      
      [LexDelete (targets) (begin
                             (display starting-tab)
                             (display "delete ")
                             (comma-separate targets)
                             (display "\n"))]
                                        ;
      [LexSubscript (left context slice)
                    (begin
                      (display starting-tab)
                      (display this-expr)
                      )]
      
      [LexListComp (body generators)
                   (display this-expr)]
      [LexComprehen (target iter)
                    (display this-expr)]
      
                                        ; builtin data structures
      [LexStr (s)  (begin (display starting-tab) (display s))]
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
                      (display " }"))]
      [LexList (values)
               (begin
                 (display starting-tab)
                 (display "[")
                 (comma-separate values)
                 (display "]"))]
      [LexSlice (lower upper step)
                (begin
                  (display this-expr)
                  )]
      [LexTuple (values) (begin
                 (display starting-tab)
                 (display "[")
                 (comma-separate values)
                 (display "]"))]
      [LexUndefined [] (begin
                         (display starting-tab)
                         (display "UNDEF"))]
      [LexSet (elts) (display this-expr)]
      [LexNone [] (begin
                    (display starting-tab)
                    (display "none"))]
      [LexBreak [] (begin
                     (display starting-tab)
                     (display "break")
                     (display "\n"))]
      [LexContinue [] (begin
                     (display starting-tab)
                     (display "continue")
                     (display "\n"))]
      [LexBlock [a b] (begin
                        (display starting-tab)
                        (display "{\n")
                        (lexexpr-print b (string-append "  " starting-tab))
                        (display "\n")
                        (display starting-tab)
                        (display "}\n"))]
      [LexImport (names asnames) (display this-expr)]
      [LexImportFrom (module names asnames level) (display this-expr)])
    this-expr
    )))
