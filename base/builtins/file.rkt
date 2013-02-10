#lang plai-typed

(require "../python-core-syntax.rkt")
(require "../util.rkt"
         "str.rkt"
         "none.rkt"
         "num.rkt"
         (typed-in "file-util.rkt"
                   (open-file : ('a string -> 'b))
                   (close-file : ('a -> void)))
         (typed-in racket/base (close-input-port : ('b -> 'void)))
         (typed-in racket/base (read-line : ('b -> 'a)))
         (typed-in racket/base (read-string : (number 'b -> 'a)))
         (typed-in racket/base (write-string : ('a 'b -> void)))
         (typed-in racket/base (eof-object? : ('a -> boolean))))

(define file-class : CExpr
  (seq-ops (list
             (CAssign (CId 'file (GlobalId))
                      (CClass
                        'file
                        (list 'object)
                        (CNone)))
             
             (def 'file '__init__
                  (CFunc (list 'self 'path 'mode) (none)
                         (CAssign
                           (CId 'self (LocalId))
                           (CBuiltinPrim 'file-open
                                         (list
                                           (CId 'path (LocalId))
                                           (CId 'mode (LocalId)))))
                         true))

             (def 'file 'read
                  (CFunc (list 'self) (some 'args)
                         (match-varargs 'args
                                        [() (CReturn (CBuiltinPrim 'file-readall
                                                                   (list
                                                                     (CId 'self (LocalId)))))]
                                        [('size) (CReturn (CBuiltinPrim 'file-read
                                                                        (list
                                                                          (CId 'self (LocalId))
                                                                          (CId 'size (LocalId)))))])
                         true))

             (def 'file 'readline
                  (CFunc (list 'self) (none)
                         (CReturn (CBuiltinPrim 'file-readline
                                                (list
                                                  (CId 'self (LocalId)))))
                         true))

             (def 'file 'write
                  (CFunc (list 'self 'data) (none)
                         (CReturn (CBuiltinPrim 'file-write
                                                (list
                                                  (CId 'self (LocalId))
                                                  (CId 'data (LocalId)))))
                         true))

             (def 'file 'close
                  (CFunc (list 'self) (none)
                         (CReturn (CBuiltinPrim 'file-close
                                                (list
                                                  (CId 'self (LocalId)))))
                         true))
             )))

(define (file-open (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'str 'str
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
  (check-types args env sto 'file 'int
               (some (make-str-value
                      (file-read-internal (MetaPort-p mval1) (MetaNum-n mval2))))))

(define (file-readall (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'file
               (some (make-str-value
                      (file-read-internal (MetaPort-p mval1) 1024)))))

(define (file-readline (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'file
               (some (make-str-value
                      (let ([line (read-line (MetaPort-p mval1))])
                        (if (eof-object? line)
                            ""
                            (string-append line "\n")))))))

(define (file-write [args : (listof CVal)] [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'file 'str
               (begin
                 (write-string (MetaStr-s mval2) (MetaPort-p mval1))
                 (some vnone))))

(define (file-close (args : (listof CVal)) [env : Env] [sto : Store]) : (optionof CVal)
  (check-types args env sto 'file
               (begin
                 (close-file (MetaPort-p mval1))
                 (some vnone))))
