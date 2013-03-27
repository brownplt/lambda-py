#lang plai-typed/untyped

(require "python-lexical-syntax.rkt")
(require [typed-in racket (pretty-write : ('a -> void))])

(define (s-map f expr) : boolean (begin (map f expr) false))
(define (comma-separate [es : (listof LexExpr)]) : boolean
  (cond
   [(empty? es) false]
   [(empty? (rest es)) (begin (lexexpr-print-helper (first es) "") false)]
   [else (begin (lexexpr-print-helper (first es) "") (display ", ") (comma-separate (rest es)) false)]))

(define (comma-separate-2 [es : (listof symbol)]) : boolean
  (cond
   [(empty? es) false]
   [(empty? (rest es)) (begin (display (first es) ) false)]
   [else (begin (display (first es) ) (display ", ") (comma-separate-2 (rest es)) false)]))

(define default-color "\033[1;37m")

(define (brown e)
  (begin
  (display "\033[1;35m")
  (display e)
  (display default-color)))
;    (display "\033[22;33mHello world!\033[1;35m")

(define (lexexpr-print expr)
  (begin
    (display default-color)
    (lexexpr-print-helper expr "")))

(define (lexexpr-print-helper [this-expr : LexExpr] [ starting-tab : string]) : LexExpr
  (let ((recur (lambda [y] [lexexpr-print-helper y starting-tab])))
  (begin
    
    (type-case LexExpr this-expr
                                        ; control structures
      [LexIf (test body orelse)
             (begin
               (display starting-tab)
               (display "if ")
               (lexexpr-print-helper test "")
               (display ":\n")
               (lexexpr-print-helper body (string-append "  " starting-tab))
               (display "else:\n")
               (lexexpr-print-helper orelse (string-append "  " starting-tab))
               (display "\n")
               this-expr)]
      [LexSeq (es) (begin
                     (display starting-tab)
                     (display "#in a sequence\n")
                     (map (lambda (y)
                                 (begin
                                   (recur y)
                                   (display "\n")
                                   )
                                 ) es) this-expr)]
                                        ; the top level seq
      [LexModule (es) (begin
                        (display starting-tab)
                        (display "module:\n")
                        (s-map (lambda [y] [lexexpr-print-helper y [string-append "  " starting-tab]]) es)
                        (display "\n")
                        this-expr)]
      [LexAssign (targets value)
                 (let ((typecase-lambda
                        (lambda ([value : LexExpr]) : LexExpr
                                (type-case LexExpr value
                                  [LexFunc (a b c d e f) (lexexpr-print-helper value starting-tab)]
                                  [LexFuncVarArg (a b c d e f) (lexexpr-print-helper value starting-tab)]
                                  [LexClass (a b c d) (lexexpr-print-helper value starting-tab)]
                                  [else (begin
                                          (display starting-tab)
                                          (comma-separate targets)
                                          (display " = ")
                                          (lexexpr-print-helper value "" )
                                          (display "\n")
                                          this-expr
                                          )]))))
                   (typecase-lambda value))]
      [LexAugAssign (op target value)
                    (begin
                      (display starting-tab)
                      (lexexpr-print-helper target "")
                      (display op)
                      (display "=")
                      (lexexpr-print-helper value "")
                      (display "\n")
                      this-expr)
                      ]
                                        ; primitive literals
      [LexNum (n) (begin (display starting-tab) (display n) this-expr)]
      [LexBool (n) (begin (display starting-tab) (display n) this-expr)]
      [LexBuiltinPrim (s args) (begin (display starting-tab) (display this-expr) this-expr)]
      [LexInstanceId (n ctx)  (begin (display starting-tab) (display "I.") (display n) this-expr)]
      [LexGlobalId (n ctx) (begin (display starting-tab) (display "G.") (display n)  this-expr)]
      [LexLocalId (n ctx) (begin (display starting-tab) (display "L.") (display n) this-expr)]
      [LexLocalLet (id bind body)
                   (begin
                     (display starting-tab)
                     (display "defvar ")
                     (display id)
                     (display " = ")
                     (lexexpr-print-helper bind "")
                     (display " in {\n")
                     (lexexpr-print-helper body (string-append "  " starting-tab))
                     (display "\n")
                     (display starting-tab)
                     (display "}\n")
                     this-expr)
                     ]
      [LexInScopeLocals ( ls )
                        (begin
                          (display starting-tab)
                          (display "# active locals: ")
                          (map (lambda (y) (begin (display y) (display ", "))) ls)
                          (display "\n")
                          this-expr)]
      [LexGlobals (ids body)
                  (if (empty? ids)
                      (recur body)
                      (begin
                        (display starting-tab)
                        (display "defvars ")
                        (comma-separate-2 ids)
                        (display " = undefined in {\n")
                        (lexexpr-print-helper body (string-append "  " starting-tab))
                        (display "\n")
                        (display starting-tab)
                        (display "}\n")
                        this-expr))]
      [PyLexId (n ctx) (begin (display starting-tab) (display n) (display " (unknown scope) ")this-expr)]
      [PyLexGlobal (ids) (begin (display starting-tab) (display "global ") (comma-separate-2 ids ) this-expr)]
      [PyLexNonLocal (ids) (begin
                             (display starting-tab)
                             (display "nonlocal ")
                             (comma-separate-2 ids)
                             (display "\n")
                             this-expr)]
      
                                        ; exceptions and exception handling
      [LexRaise (expr) (begin (display starting-tab) (display "raise ") (lexexpr-print-helper expr "") (display "\n")this-expr) ]
      [LexExcept (types  body)
                 (begin
                   (display starting-tab)
                   (display "except ")
                   (comma-separate types)
                   (display ":\n")
                   (lexexpr-print-helper body (string-append "  " starting-tab))
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
                   (lexexpr-print-helper body (string-append "  " starting-tab))
                   (display "\n")
                   this-expr)]
      [LexTryExceptElse (try except orelse)
                        (begin
                        (display starting-tab)
                        (display "try:\n")
                        (lexexpr-print-helper try (string-append "  " starting-tab))
                        (display "\n")
                        (map recur except)
                        (display "\n")
                        (display "else:")
                        (lexexpr-print-helper orelse (string-append "  " starting-tab))
                        (display "\n")
                        this-expr)]
      [LexTryFinally (try finally)
                     (begin
                       (display starting-tab)
                       (display "try:\n")
                       (lexexpr-print-helper try (string-append "  " starting-tab))
                       (display "\n")
                       (display starting-tab)
                       (display "finally:")
                       (lexexpr-print-helper finally (string-append "  " starting-tab))
                       (display "\n")
                       this-expr)]

      [LexYield (expr) (begin (display starting-tab) (display "yield ") (lexexpr-print-helper expr "") this-expr)]
                                        ;loops 
      [LexWhile (test body orelse)
                (begin
                  (display starting-tab)
                  (display "while ")
                  (lexexpr-print-helper test "")
                  (display ":\n")
                  (lexexpr-print-helper body (string-append "  " starting-tab))
                  (display "\n")
                  (display starting-tab)
                  (display "else:\n")
                  (lexexpr-print-helper orelse (string-append "  " starting-tab))
                  (display "\n")
                  this-expr)]
                  
      [LexFor (target iter body)
              (begin
                (display starting-tab)
                (display "for ")
                (lexexpr-print-helper target "")
                (display " in ")
                (lexexpr-print-helper iter "")
                (display ":\n")
                (lexexpr-print-helper body (string-append "  " starting-tab))
                 this-expr)]

      
                                        ; pass & assert
      [LexPass () (begin
                    (display starting-tab)
                    (display "pass")
                    (display "\n")
                    this-expr)]
      [LexCore (_) (begin (pretty-write this-expr) this-expr)]
      [LexAssert (test msg)
                 (begin (display this-expr) this-expr)]
      
                                        ; classes and objects 
      [LexClass (scope name bases body)
                (begin
                  (display starting-tab)
                  (display "class ")
                  (display name)
                  (display "(")
                  (lexexpr-print-helper bases "")
                  (display "):")
                  (display "\n")
                  (lexexpr-print-helper body (string-append "  " starting-tab))
                  this-expr)]
      [LexDotField (value attr)
                   (begin
                     (lexexpr-print-helper value starting-tab)
                     (display ".")
                     (display attr )
                     this-expr)]
      [LexExprField (value attr)
                    (begin
                      (display this-expr)
                      this-expr)
                    ]
      [LexExprAssign (obj attr value)
                    (begin
                      (display this-expr)
                      this-expr)
                    ]
      
                                        ; operations
      [LexBinOp (left op right)
                (begin
                  (display starting-tab)
                  (lexexpr-print-helper left "")
                  (display " ")
                  (display op)
                  (display " ")
                  (lexexpr-print-helper right "")
                  this-expr
                  )] 
      [LexUnaryOp (op operand) (begin
                                 (display starting-tab)
                                 (display op)
                                 (lexexpr-print-helper operand "")
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
                (lexexpr-print-helper body (string-append " " starting-tab))
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
                 (lexexpr-print-helper body (string-append "  " starting-tab))
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
                       (lexexpr-print-helper body (string-append "  " starting-tab))
                       (display "\n")
                                        ;END TAB
                       this-expr)]
      [LexReturn (value) (begin
                           (display "\n")
                           (display starting-tab)
                           (display "return ")
                           (type-case (optionof LexExpr) value
                             [some (value) (lexexpr-print-helper value "")]
                             [none [] this-expr])
                           (display "\n")
                           this-expr)]
      [LexApp (fun args) (begin
                           (lexexpr-print-helper fun starting-tab)
                           (display "(")
                           (comma-separate args)
                           (display ")")
                           this-expr)]
      [LexAppStarArg (fun args stararg)
                     (begin
                       (display this-expr)
                       this-expr)]
      
      [LexDelete (targets) (begin
                             (display "\n")
                             (display starting-tab)
                             (display "delete ")
                             (comma-separate targets)
                             (display "\n")
                             this-expr)]
                                        ;
      [LexSubscript (left context slice)
                    (begin
                      (display starting-tab)
                      (lexexpr-print-helper left "")
                      (display "[")
                      (lexexpr-print-helper slice "")
                      (display "]")
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
      [LexStr (s)  (begin (display starting-tab) (display "'") (display s) (display "'") this-expr)]
      [LexDict (keys values)
               (begin (display starting-tab)
                      (display "{ ")
                      (local
                       [(define (hlpr keys values) : LexExpr
                          (cond
                           [(empty? keys) (begin (display "") this-expr) ]
                           [(empty? (rest keys))
                            (begin
                              (lexexpr-print-helper (first keys) "")
                              (display " : ")
                              (lexexpr-print-helper (first values) "")
                              )]
                           [else (begin
                                   (lexexpr-print-helper (first keys) "")
                                   (display " : ")
                                   (lexexpr-print-helper (first values) "")
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
                        (brown "{")
                        (display "\n")
                        (lexexpr-print-helper b (string-append "  " starting-tab))
                        (display "\n")
                        (display starting-tab)
                        (brown "}")
                        (display "\n")
                        this-expr)]
      [LexImport (names asnames) (begin
                                   (display this-expr)
                                   this-expr)]
      [LexImportFrom (module names asnames level) (begin
                                                    (display this-expr)
                                                    this-expr)])
    this-expr
    )))
