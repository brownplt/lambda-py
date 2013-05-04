#lang racket

(require rackunit
         "python-parser.rkt")

;; Emacs help writing these: https://gist.github.com/jmillikan/5478028

;; follow-parse-path is for retrieveing a specific interior AST from a parse.
;; Follow a "path" where each step is an AST type and key (and index if value at key is an AST list)
;; Unwrap SrcLoc ASTs unless at the end of the path
;; Path example: '(("Module" body 0) ("FunctionDef" body 0) ("If" orelse))
(define (follow-parse-path ast path)
  (match path
    ['() ast]
    [`((,path-nt ,path-key ,path-index-or-null ...) ,path-rest ...)
     (if (equal? "SrcLoc" (hash-ref ast 'nodetype))
         (follow-parse-path (hash-ref ast 'ast) path)
         (begin
           (unless (equal? path-nt (hash-ref ast 'nodetype))
             (error (format "Wrong nodetype following path - expected ~a, got ~a" 
                            path-nt
                          (hash-ref ast 'nodetype))))
           (follow-parse-path 
            (if (null? path-index-or-null)
                (hash-ref ast path-key) 
                (list-ref (hash-ref ast path-key) (first path-index-or-null)))
            path-rest)))]))

;; Get a src-loc AST of program in string s
(define (parse s)
  (parameterize [(parser-src-loc #t)]
    (parse-python (open-input-string s))))

;; Check for SrcLoc AST found found at 'path' in AST 'parsed-prog' with given attributes
(define (check-src-loc parsed-prog path line column position span)
  (let ((target-ast (follow-parse-path parsed-prog path)))
    (check-equal? (hash-ref target-ast 'nodetype) "SrcLoc" (format "Not a SrcLoc (~a)" position))
    (check-equal? (map (lambda (k) (hash-ref target-ast k))
                       '(line column position span))
                  (list line column position span))))

;; Line: 1-based line number of start of construct
;; Column: 0-based character offset into line
;; Position: 1-based character offset into file
;; Span: Statements with suites will end with the first newline after the last stmt
;; or the last token before EOF. Some here strings have extra newlines to keep this
;; straightforward.

;; Note: These don't really belong in test-begin. Will fix once I take time to get RackUnit.

(test-begin 
 "Test that in-def statements parse with expected source locations"
 (let ((prog (parse 
#<<EOF
def f():
  yield 1
  return
  return True
EOF
)))
   (check-src-loc prog '(("Module" body 0) ("FunctionDef" body 0)) 2 2 12 7)
   (check-src-loc prog '(("Module" body 0) ("FunctionDef" body 1)) 3 2 22 6)
   (check-src-loc prog '(("Module" body 0) ("FunctionDef" body 2)) 4 2 31 11)
))

(test-begin
 "Test that in-loop statements parse with expected source locations"
 (let ((prog (parse "while True:\n  continue\n  break")))
   (check-src-loc prog '(("Module" body 0) ("While" body 0)) 2 2 15 8) ;; continue
   (check-src-loc prog '(("Module" body 0) ("While" body 1)) 3 2 26 5) ;; break
))

(test-begin
 "Test srcloc of value, assign & augassign statements"
 (let ((prog (parse "1\na = 1\na = b = 1\na += 1\n")))
   (check-src-loc prog '(("Module" body 0)) 1 0 1 1) ;; expr
   (check-src-loc prog '(("Module" body 1)) 2 0 3 5) ;; assign
   (check-src-loc prog '(("Module" body 2)) 3 0 9 9) ;; multi-target
   (check-src-loc prog '(("Module" body 3)) 4 0 19 6) ;; augassign
))
   
(test-begin
 "Test srcloc of some bare simple statements"
 (let ((prog (parse "del a;raise;raise b from c;assert True")))
   (check-src-loc prog '(("Module" body 0)) 1 0 1 5)
   (check-src-loc prog '(("Module" body 1)) 1 6 7 5)
   (check-src-loc prog '(("Module" body 2)) 1 12 13 14)
   (check-src-loc prog '(("Module" body 3)) 1 27 28 11)
))

(test-begin
 "Global, nonlocal & pass"
 (let ((prog (parse 
#<<EOF
def f():
  y = 0
  def g():global x;nonlocal y
  def h():pass
EOF
))) ;; This depends on the source here retaining spaces and Unix newlines
   (check-src-loc prog '(("Module" body 0) ("FunctionDef" body 1) ("FunctionDef" body 0)) 3 10 28 8)
   (check-src-loc prog '(("Module" body 0) ("FunctionDef" body 1) ("FunctionDef" body 1)) 3 19 37 10)
   (check-src-loc prog '(("Module" body 0) ("FunctionDef" body 2) ("FunctionDef" body 0)) 4 10 58 4)
))

(test-begin
 "Single and multi-line with"
 (let ((prog (parse
#<<EOF
with a:pass
with b, c as d:
  pass

EOF
)))
  (check-src-loc prog '(("Module" body 0)) 1 0 1 12) ;; 12 here: suite -> simple_stmt which includes newline
  (check-src-loc prog '(("Module" body 1)) 2 0 13 23) ;; 2nd with statement - entire
  (check-src-loc prog '(("Module" body 1) ("With" body 0)) 2 0 13 23) ;; 2nd with statement - second clause.
  ;; Same as the whole statement. This is "correct" for now.
))

(test-begin
 "if, elif and else"
 (let ((prog (parse
#<<EOF
if a: pass
elif b: pass
else: pass

EOF
)))
   (check-src-loc prog '(("Module" body 0)) 1 0 1 35) ;; if
   (check-src-loc prog '(("Module" body 0) ("If" orelse 0)) 2 0 12 24) ;; elif
   ;; Else clause has no specific tracking for now, as there's nowhere to put it...
))   

(test-begin
 "Import, ImportFrom and individual names in ImportFrom"
 (let ((prog (parse "from a import b as c;import d")))
   (check-src-loc prog '(("Module" body 0)) 1 0 1 20)  ;; from...import
   (check-src-loc prog '(("Module" body 1)) 1 21 22 8) ;; import...
   (check-src-loc prog '(("Module" body 0) ("ImportFrom" names 0)) 1 14 15 6) ;; b as c
))

(test-begin
 "Try with except, else, finally"
 (let ((prog (parse
#<<EOF
try: pass
except p: pass
except q: pass
except: pass
else: pass
finally: pass

try: pass
except: pass

EOF
)))
   (check-src-loc prog '(("Module" body 0)) 1 0 1 78) ;; Entire first try
   (check-src-loc prog '(("Module" body 0) ("TryFinally" body 0) ("TryExcept" handlers 0))
                        2 0 11 15) ;; except p
   (check-src-loc prog '(("Module" body 0) ("TryFinally" body 0) ("TryExcept" handlers 2))
                        4 0 41 13) ;; except
   
   (check-src-loc prog '(("Module" body 1)) 8 0 80 23) ;; Entire second try
   (check-src-loc prog '(("Module" body 1) ("TryExcept" handlers 0)) 9 0 90 13) ;; except
   
   ;; else/finally don't have an AST, so no special SrcLoc for now...
))

(test-begin
 "While and for loops"
 (let ((prog (parse
#<<EOF
while True:pass
for a in b:pass
for c in d:pass
else: pass

EOF
)))
   (check-src-loc prog '(("Module" body 0)) 1 0 1 16)
   (check-src-loc prog '(("Module" body 1)) 2 0 17 16)
   (check-src-loc prog '(("Module" body 2)) 3 0 33 27)
))

(test-begin
 "Functions, decorated functions, arguments and individual arguments, and lambda arguments (unfortunately)"
 (let ((prog (parse
#<<EOF
def f(a):pass
@g
def h(b):pass
lambda x:True
EOF
)))
   ;; TEMP: 'arguments' and 'names' temporarily removed to keep get-structured-python simple
   (check-src-loc prog '(("Module" body 0)) 1 0 1 14) ;; f
   ;; "arguments" ast spans entire function
   (check-src-loc prog '(("Module" body 0) ("FunctionDef" args)) 1 0 1 14) 
   (check-src-loc prog '(("Module" body 0) ("FunctionDef" args) ("arguments" args 0))
                  1 6 7 1) ;; a

   (check-src-loc prog '(("Module" body 1)) 2 0 15 17) ;; h, including decorators
   ;; "arguments", again, maches entire def AST, including decorators
   (check-src-loc prog '(("Module" body 1) ("FunctionDef" args)) 2 0 15 17)
   (check-src-loc prog '(("Module" body 1) ("FunctionDef" args) ("arguments" args 0))
                  3 6 24 1) ;; b

   (check-src-loc prog '(("Module" body 2) ("Expr" value) ("Lambda" args))
                  4 0 32 13)
   ;; lambdas aren't covered yet (4/28/13) but their arguments are...
))

(test-begin
 "Classes, decorated classes, 'args' to classes"
 (let ((prog (parse
#<<EOF
class x(y,z=a,*b,**c): pass
@k()
@l
class d(e): pass

EOF
)))
   (check-src-loc prog '(("Module" body 0)) 1 0 1 28) ;; class x
   (check-src-loc prog '(("Module" body 1)) 2 0 29 25) ;; class d
   ;; decorator call to k - includes newline
   (check-src-loc prog '(("Module" body 1) ("ClassDef" decorator_list 0)) 2 0 29 5)
   ;; decorator l (name only) - only includes name. This is not ideal wrt other decorators...
   (check-src-loc prog '(("Module" body 1) ("ClassDef" decorator_list 1)) 3 1 35 1)
   ;; TODO: Optional check for final (unwrapped) AST nodetype in check-src-loc, for readability and least surprise
))

(test-begin
 "Expressions 1"
 (let ((prog (parse "a,b,1;c if d else e;lambda:f;yield g;yield;a < b;d + e;f or g")))
   (check-src-loc prog '(("Module" body 0) ("Expr" value)) 1 0 1 5) ; tuple
   (check-src-loc prog '(("Module" body 1) ("Expr" value)) 1 6 7 13) ; if
   (check-src-loc prog '(("Module" body 2) ("Expr" value)) 1 20 21 8) ; lambda
   (check-src-loc prog '(("Module" body 3) ("Expr" value)) 1 29 30 7) ; yield
   (check-src-loc prog '(("Module" body 4) ("Expr" value)) 1 37 38 5) ; yield expr
   (check-src-loc prog '(("Module" body 5) ("Expr" value)) 1 43 44 5) ; comp
   (check-src-loc prog '(("Module" body 6) ("Expr" value)) 1 49 50 5) ; binops
   (check-src-loc prog '(("Module" body 7) ("Expr" value)) 1 55 56 6) ; andor
))

(test-begin
 "power and trailers"
 (let ((prog (parse "a ** b;c[d];e(f);g[h](i)")))
   (check-src-loc prog '(("Module" body 0) ("Expr" value)) 1 0 1 6) ; pow
   (check-src-loc prog '(("Module" body 1) ("Expr" value)) 1 7 8 4) ; subscript
   (check-src-loc prog '(("Module" body 2) ("Expr" value)) 1 12 13 4) ; call
   (check-src-loc prog '(("Module" body 3) ("Expr" value)) 1 17 18 7) ; g[h](i)
   (check-src-loc prog '(("Module" body 3) ("Expr" value) ("Call" func)) 1 17 18 4) ; [h]
   ;; Might want to change trailers to include the value/function
))


