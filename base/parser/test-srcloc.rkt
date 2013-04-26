#lang racket

(require rackunit
         "python-parser.rkt")

;; follow-parse-path is for retrieveing a specific interior AST from a parse.
;; Follow a "path" where each step is an AST type and key (and index if value at key is an AST list)
;; Unwrap SrcLoc ASTs unless at the end of the path
;; Path example: '(("Module" body 0) ("FunctionDef" body 0) ("If" orelse))
(define (follow-parse-path ast path)
  (match path
    ['() ast]
    [`((,path-nt ,path-key ,path-index-or-null ...) ,path-rest ...)
     (let ((real-ast ;; Unwrap SrcLoc if current path isn't null
            (if (equal? "SrcLoc" (hash-ref ast 'nodetype))
                (hash-ref ast 'ast)
                ast)))
       (unless (equal? path-nt (hash-ref real-ast 'nodetype))
         (error (format "Wrong nodetype following path - expected ~a, got ~a" 
                        path-nt
                        (hash-ref real-ast 'nodetype))))
       (follow-parse-path 
        (if (null? path-index-or-null)
            (hash-ref real-ast path-key) 
            (list-ref (hash-ref real-ast path-key) (first path-index-or-null)))
        path-rest))]))

;; Get a src-loc AST of program in string s
(define (parse s)
  (parameterize [(parser-src-loc #t)]
    (parse-python (open-input-string s))))

;; Check for SrcLoc AST found found at 'path' in AST 'parsed-prog' with given attributes
(define (check-src-loc parsed-prog path line column position span)
  (let ((target-ast (follow-parse-path parsed-prog path)))
    (check-equal? "SrcLoc" (hash-ref target-ast 'nodetype) "Not a SrcLoc")
    (check-equal? position (hash-ref target-ast 'position) "Incorrect SrcLoc position")
    (check-equal? line (hash-ref target-ast 'line) "Incorrect SrcLoc line")
    (check-equal? column (hash-ref target-ast 'column) "Incorrect SrcLoc column")
    (check-equal? span (hash-ref target-ast 'span) "Incorrect SrcLoc span")))

;; Line is 1-based
;; Column is 0-based
;; Position is 1-based
;; Span: Statements with suites will end with the last possible terminating newline, I believe. Not yet checked.

;; Note: These don't really belong in test-begin. I should rebuild check-src-loc as a 
;; define-check that explicitly checks all four numbers at once...

(test-begin 
 "Test that in-def statements parse with expected source locations"
 (let ((prog (parse "def f():\n  yield 1\n  return\n  return True")))
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
 (let ((prog (parse "del a;raise;raise b from c")))
   (check-src-loc prog '(("Module" body 0)) 1 0 1 5)
   (check-src-loc prog '(("Module" body 1)) 1 6 7 5)
   (check-src-loc prog '(("Module" body 2)) 1 12 13 14)
))

