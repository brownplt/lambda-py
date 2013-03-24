#lang plai-typed/untyped

(require "../python-core-syntax.rkt")
(require "../util.rkt"
         "str.rkt"
         "num.rkt"
         (typed-in "file-util.rkt"
                   (open-file : ('a string -> 'b))
                   (close-file : ('a -> void)))
         (typed-in racket/base (close-input-port : ('b -> 'void)))
         (typed-in racket/base (read-line : ('b -> 'a)))
         (typed-in racket/base (read-string : (number 'b -> 'a)))
         (typed-in racket/base (write-string : ('a 'b -> void)))
         (typed-in racket/base (eof-object? : ('a -> boolean)))
         (typed-in racket/base (file-exists? : ('a -> 'b))))

(define (file-open (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaStr? MetaStr?
               (let ([filename (MetaStr-s mval1)]
                     [mode (MetaStr-s mval2)])
                     (some (VObject 'file
                                    (some (MetaPort (open-file filename mode)))
                                    (hash empty))))))

(define (file-read-internal [file : port] [size : number]) : string
  (let ([s (read-string size file)])
    (if (eof-object? s)
        ""
        s)))

(define (file-readall-internal [file : port]) : string
  (let ([s (read-string 1024 file)])
    (if (eof-object? s)
        ""
        (string-append s (file-readall-internal file)))))

(define (file-read (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaPort? MetaNum?
               (some (make-str-value
                      (file-read-internal (MetaPort-p mval1) (MetaNum-n mval2))
                      (third args)))))

(define (file-readall (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaPort?
               (some (make-str-value
                      (file-read-internal (MetaPort-p mval1) 1024)
                      (second args)))))

(define (file-readline (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaPort?
               (some (make-str-value
                      (let ([line (read-line (MetaPort-p mval1))])
                        (if (eof-object? line)
                            ""
                            (string-append line "\n")))
                      (second args)))))

(define (file-write [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaPort? MetaStr?
               (begin
                 (write-string (MetaStr-s mval2) (MetaPort-p mval1))
                 (some vnone))))

(define (file-close (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaPort?
               (begin
                 (close-file (MetaPort-p mval1))
                 (some vnone))))

(define (existing-file? [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types-pred args env sto MetaStr?
               (if (file-exists? (MetaStr-s mval1))
                   (some true-val)
                   (some false-val))))
