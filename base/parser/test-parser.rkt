#lang racket

(require racket/pretty
         "parser.rkt"
         "../run-tests.rkt" ;; borrow get-test-specs and TestSpec
         "../util.rkt")

(provide parse-tests)

;; For single files, a bash script like this is useful:
;; diff <(./pymain.sh --native-parser --get-syntax < $1) <(./pymain.sh --python-parser --get-syntax < $1)

(define (parse-tests dirname)
  (define specs (get-test-specs dirname))
  (for ([spec specs])
    (match (compare-parse (open-input-string (TestSpec-program-src spec)))
      [(list 'both-parsers-fail py-exn native-exn) 

       (display (TestSpec-program-name spec)) (newline)
       (display "Both parsers fail\n")]
      [(list 'python-parser-fails py-exn _) 
       (display (TestSpec-program-name spec)) (newline)
       (display "Python parser fails\n")]
      [(list 'native-parser-fails _ _) 
       (display (TestSpec-program-name spec)) (newline)
       (display "Native parser fails\n")]
      ['equal '()]
      [(list 'not-equal native py)
       (display (TestSpec-program-name spec)) (newline)
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
     [else (list 'not-equal python-ast-or-exn native-ast-or-exn)])))
        ;; parser currently displays unhandled portion
