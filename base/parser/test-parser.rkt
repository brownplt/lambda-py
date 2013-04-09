#lang racket

(require racket/pretty
         "parser.rkt"
         "../run-tests.rkt" ;; borrow get-test-specs and TestSpec
         "../util.rkt")

(provide single-parse-test parse-tests)

; Take a port, provide its contents to native parser and python parser
; If equal?, return success message, otherwise a failure message wiht both ASTs

(define (single-parse-test port)
  (match (compare-parse port)
    [(list 'both-parsers-fail _ _) (display "Both parsers fail\n") (exit 1)]
    [(list 'python-parser-fails _ _) (display "Python parser fails\n") (exit 2)]
    [(list 'native-parser-fails _ _) (display "Native parser fails\n") (exit 3)]
    ['equal (display "Programs are equal\n") (exit 0)]
    [(list 'not-equal native py)
     (display "Native parser does not match python parser\n")
     (display "=== Native ===\n")
     (pretty-write native)
     (display "=== Python ===\n")
     (pretty-write py)]))

(define (parse-tests dirname)
  (define specs (get-test-specs dirname))
  (for ([spec specs])
    (display (TestSpec-program-name spec)) (newline)
    (match (compare-parse (open-input-string (TestSpec-program-src spec)))
      [(list 'both-parsers-fail _ _) (display "Both parsers fail\n")]
      [(list 'python-parser-fails _ _) (display "Python parser fails\n")]
      [(list 'native-parser-fails _ _) (display "Native parser fails\n")]
      ['equal '()]
      [(list 'not-equal native py)
       (display "Native parser fails to match python parser\n")])))

(define (compare-parse port)
  (let* ((program-text (port->string port))
         (program-port-1 (open-input-string program-text))
         (program-port-2 (open-input-string program-text))	 
         (python-ast-or-exn
          (with-handlers [(exn:fail? (lambda (e) e))]
            (parameterize [(parser python-parse-python/port)]
              ((parser) program-port-1))))
         (native-ast-or-exn 
          (with-handlers [(exn:fail? (lambda (e) e))]
            (parameterize [(parser native-parse-python/port)]
              ((parser) program-port-2)))))
    (cond
     [(and (exn:fail? python-ast-or-exn)
           (exn:fail? native-ast-or-exn))
      (list 'both-parsers-fail python-ast-or-exn native-ast-or-exn)]
     [(exn:fail? python-ast-or-exn)
      (list 'python-parser-fails python-ast-or-exn native-ast-or-exn)]
     [(exn:fail? native-ast-or-exn)
      (list 'native-parser-fails python-ast-or-exn native-ast-or-exn)]
     [(equal? native-ast-or-exn python-ast-or-exn) 
      'equal]
     [else (list 'not-equal native-ast-or-exn python-ast-or-exn)])))
        ;; parser currently displays unhandled portion
