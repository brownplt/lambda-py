#lang plai

; Didn't know how to import open-output-file in to
; plai-typed as it had some optional keyword arguments.
;
; Working around that problem by defining this module in #lang racket.

(define (open-file path mode)
  (cond
    [(equal? mode "w") (open-output-file path #:exists 'replace)]
    [(equal? mode "a") (open-output-file path #:exists 'append)]
    [else (open-input-file path)]))

(define (close-file port)
  (if (output-port? port)
      (close-output-port port)
      (close-input-port port)))

