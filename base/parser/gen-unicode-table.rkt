#lang racket

(define unicode-data (open-input-file "parser/UnicodeData.txt"))

(display "#lang racket\n\

(provide unicode-char-names)

(define 
 unicode-char-names
 (hash
")

(let next-line ((line (read-line unicode-data)))
  (unless (eof-object? line)
    (let ([m (regexp-match #rx"^([0-9A-F]+);([^;]*);([^;]*);([^;]*);[^;]*;([^;]*);[^;]*;([^;]*);[^;]*;[^;]*;[^;]*;[^;]*;([^;]*);([^;]*);([^;]*)"
                           line)])
      (let ((name (list-ref m 2))
            (code (list-ref m 1)))
        (unless (equal? #\< (string-ref name 0))
          (display name (current-error-port))
          (display (string-append "  \"" name "\"\t#\\U" code "\n"))) ;; TODO NOW: FORMAT
        (next-line (read-line unicode-data))))))
(display "))")
