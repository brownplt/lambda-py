#lang racket

(provide json-summary run-test-specs-from-s-exp
         serialize-test-suite run-tests results-summary simple-summary [struct-out Result])
(require racket/set)

(struct Result (name timeout? expected-out actual-out expected-err actual-err)
               #:transparent)
;; (TestSpec string string string string)
(struct TestSpec (program-name program-src output error) #:transparent)


(define INTERP-TIMEOUT-SECONDS 3)

;; run-for-n-seconds :: (-> (pairof string string)) number ->
;;                      (pairof string string) U #f
(define (run-for-n-seconds thnk n)
  (define current (current-thread))
  (define (wrapped-thunk)
    (define result (thnk))
    (thread-send current result))
  (define handle (thread wrapped-thunk))
  (define receive (thread-receive-evt))
  (define got-result (sync/timeout n receive))
  (define result (if got-result (thread-receive) #f))
  (when (not got-result) (break-thread handle))
  result)

(define (clean s) (regexp-replace* "\r" s ""))
  
;; run-test-spec : (string port -> (pairof string string)) TestSpec -> Result
(define (run-test-spec interp test-spec)
  (define interp-thunk
    (λ ()
      (define srcport (open-input-string (TestSpec-program-src test-spec)))
      (define result (interp (TestSpec-program-name test-spec) srcport))
      (close-input-port srcport)
      result))
  (define interp-output
    (run-for-n-seconds interp-thunk INTERP-TIMEOUT-SECONDS))
  ;; Check for a false return from run-for-n-seconds, which indicates
  ;; a timeout happened
  (define-values (stdout stderr)
    (cond [(false? interp-output) (values "TIMEOUT" "TIMEOUT")]
          [(cons? interp-output)
           (values (car interp-output) (cdr interp-output))]))
  (define timed-out? (not interp-output))
  (define-values (_ name __) (split-path (TestSpec-program-name test-spec)))
  (Result name
          timed-out?
          (TestSpec-output test-spec)
          stdout
          (TestSpec-error test-spec)
          stderr))

;; path->test-spec : some-system-path -> TestSpec
(define (path->test-spec path)
  (define strpath (some-system-path->string path))
  (define expected-path (string-append strpath ".expected"))
  (define err-path (string-append strpath ".error"))
  (define (contents-or-empty f)
    (define p (if (file-exists? f) (open-input-file f) (open-input-string "")))
    (define contents (port->string p))
    (close-input-port p)
    (clean contents))
  (define expected-out (contents-or-empty expected-path))
  (define expected-err (contents-or-empty err-path))
  (define src-input (open-input-file (some-system-path->string path)))
  (define src (port->string src-input))
  (close-input-port src-input)
  (TestSpec strpath src expected-out expected-err))

(define (test-spec->s-exp test-spec)
  ;; we ditch path information
  (define-values (_ name __) (split-path (TestSpec-program-name test-spec)))
  (list (path->string name)
        (TestSpec-program-src test-spec)
        (TestSpec-output test-spec)
        (TestSpec-error test-spec)))

(define (s-exp->test-spec s-exp)
  (TestSpec (first s-exp) (second s-exp) (third s-exp) (fourth s-exp)))

(define (test-specs->racket-str test-specs)
  (define out (open-output-string))
  (write (map test-spec->s-exp test-specs) out)
  (get-output-string out))

(define (serialize-test-specs test-specs)
  (define template "#lang racket/base\n\n(provide TESTSPECS)\n\n(define TESTSPECS '~a)\n")
  (format template (test-specs->racket-str test-specs)))

(define (serialize-test-suite dirname)
  (serialize-test-specs (get-test-specs dirname)))

(define (deserialize-test-suite s-exps)
  (map s-exp->test-spec s-exps))

;; get-test-specs : path -> (listof test-spec)
(define (get-test-specs dirname)
  (define EXTENSION ".py")

  (define (directory-list-with-name dirname)
    (map (λ (p) (path->complete-path p dirname))
      (directory-list (some-system-path->string dirname))))

  (define (syspath-dir-exists? dirname)
    (directory-exists? (some-system-path->string dirname)))

  (define (parseltongue-paths dirname)
    (define paths (directory-list-with-name dirname))
    (define (plt? path)
      (define strpath (some-system-path->string path))
      (and
        (not (directory-exists? (some-system-path->string path)))
        (equal? EXTENSION (substring strpath (- (string-length strpath)
                                                (string-length EXTENSION))))))
    (append* (cons (filter plt? paths)
             (map parseltongue-paths (filter syspath-dir-exists? paths)))))

  (when (not (directory-exists? dirname))
        (error (format "Directory not found: ~a" dirname)))
  (for/list ([path (parseltongue-paths (path->complete-path dirname))])
    (path->test-spec path)))


(define (run-test-specs-from-s-exp interp s-exp)
  (define specs (deserialize-test-suite s-exp))
  (for/list ([spec specs])
    (run-test-spec interp spec)))

;; run-tests : (string port -> (pairof string string)) string ->
;;             (listof Result)
#|

  interp - An interpreter that accepts a filename and a port to read a
           source file from, and yields the "stdout" and "stderr" of
           the evaluator

  dir-name - A root directory full of tests to run that looks like:

             dir-name/
                test1.psl
                test1.psl.expected
                test1.psl.error
                test3.psl
                test3.psl.expected
                subdir/
                  test2.psl
                  test2.psl.expected 
                subdir2/
                  subdir3/
                    test4.psl
                    test4.psl.error

            run-tests will use interp to run test1.psl, test2.psl, etc,
            and check the standard out/error of the interpreter against
            the corresponding .expected and .error files.  If a
            .expected or .error file is omitted, it is assumed to be the
            empty string.

|#
(define (run-tests interp dirname)
  (define specs (get-test-specs dirname))
  (for/list ([spec specs])
    (run-test-spec interp spec)))

(define (successful-result r)
  (and (Result? r)
             (equal? (Result-actual-out r) (Result-expected-out r))
             (equal? (Result-actual-err r) (Result-expected-err r))))

(define (partition-results results)
  (partition successful-result results))

(define (results-summary results)

  (define report-string (open-output-string "report"))
  (define (report str . args)
    (display (apply format (cons str args)) report-string))

  (define-values (passed failed) (partition-results results))

  (report "~a tests succeeded.\n" (length passed))
  (report "~a tests failed.\n" (length failed))

  (when (not (empty? failed)) (report "== Output of failed tests ==\n"))
  (for ((f failed))
    (report "=====================================================\n")
    (report "=   Results for ~a   =\n" (Result-name f))
    (report "=====================================================\n")
    (when (Result-timeout? f) (report "TEST TIMED OUT\n"))
    (report "=== Expected stdout ===\n~a\n" (Result-expected-out f))
    (report "=== Actual stdout ===\n~a\n" (Result-actual-out f))
    (report "=== Expected stderr ===\n~a\n" (Result-expected-err f))
    (report "=== Actual stderr ===\n~a\n" (Result-actual-err f)))

  (get-output-string report-string))

(define (simple-summary results)
  (define report-string (open-output-string "report"))
  (define (report str . args)
    (display (apply format (cons str args)) report-string))

  (define-values (passed failed) (partition-results results))

  (for/fold ((str "Differences in:\n"))
            ((r failed))
    (string-append str (format "~a\n" (Result-name r)))))

(define (json-summary results)
  (for/hash ((result results))
    (values (Result-name result) (successful-result result))))


