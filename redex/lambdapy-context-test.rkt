#lang racket

(require "lambdapy-test-util.rkt")

(define basic-store (term
  {(1 (pointer-val 2))
   (2 (obj-val %function (meta-closure (λ () (no-var)
                                          (alloc
                                           (obj-val BaseException (no-meta)
                                                    {("__mro__" 9)})))) {}))
   (3 vnone)
   (4 vnone)
   (5 vnone)
   (6 vtrue)
   (7 (pointer-val 8))
   (8 (obj-val %function (meta-closure (λ (o1 o2) (no-var)
                                         (builtin-prim "isinstance" ((id o1 local) (id o2 local)))))
               {}))
   (9 (pointer-val 10))
   (10 (obj-val %tuple (meta-tuple ((pointer-val 11))) {}))
   (11 (obj-val vnone (no-meta) {}))
   (12 (pointer-val 11))
   (13 vfalse)
   (14 vnone)
   (15 vnone)}))

(define basic-env (term
                   {(Exception 1)
                    (%str 3)
                    (%locals 4)
                    (%int 5)
                    (True 6)
                    (False 13)
                    (%isinstance 7)
                    (BaseException 12)
                    (%globals 14)
                    (None 15)}))
(full-expect
 ((app
   (fun (f) (no-var)
         (return (builtin-prim "str-getitem"
                               ((app (id f local) ())
                                (mknum 0)))))
   ((fun () (no-var)
          (seq
           (return ,(redex-strv "get-my-g"))
           (raise (sym "should not reach"))))))
  () ())
 ((obj-val any_cls (meta-str "g") any_dict) ε Σ))

(full-expect
 ((return (sym "top return")) () ())
 ((err (sym "ill-formed-return")) () ()))

(full-expect
 ((return (return (sym "stuck"))) () ())
 ((err (sym "ill-formed-return")) () ()))

(full-expect
 ((app
   (fun () (no-var)
        (tryexcept
         (return (sym "return-through-try"))
         x
         (raise (sym "should not reach"))
         (raise (sym "should not reach")))) ())
  ()())
 ((sym "return-through-try") ε Σ))

(full-expect
 ((app
   (fun () (no-var)
        (tryfinally
         (return (sym "return-through-finally"))
         (return (sym "return-from-finally")))) ())
  ()())
 ((sym "return-from-finally") ε Σ))

(full-expect
 ((let (check-var global = (sym "init")) in
    (app
     (fun () (no-var)
          (tryfinally
           (return (sym "return-after-finally"))
           (assign (id check-var global) := (sym "saw finally")))) ()))
  ()())
 ((sym "return-after-finally")
  {(x ref) ... (check-var ref_chk) (x_n ref_n) ...}
  {(ref val) ... (ref_chk (sym "saw finally")) (ref_rest val_rest) ...}))

(full-expect
 ((let (check-var global = (sym "init")) in
    (app
     (fun () (no-var)
          (tryfinally
           (raise (sym "raise-after-finally"))
           (assign (id check-var global) := (sym "saw finally")))) ()))
  ()())
 ((err (sym "raise-after-finally"))
  {(x ref) ... (check-var ref_chk) (x_n ref_n) ...}
  {(ref val) ... (ref_chk (sym "saw finally")) (ref_rest val_rest) ...}))

(full-expect
 (,(python->redex "
chk = 0
try:
  raise Exception()
finally:
  chk = 10
")
   ,basic-env
   ,basic-store)
 (side-condition
  ((err (pointer-val ref_exn))
   ((x ref) ... (chk ref_chk) (x_n ref_n) ...)
   (name Σ ((ref_1 val_1) ... (ref_chk (pointer-val ref_chkval)) (ref_rest val_rest) ...)))
  (and
   (redex-match λπ (obj-val any_cls (meta-num 10) any_dict)
                (term (store-lookup Σ ref_chkval)))
   (redex-match λπ (obj-val BaseException (no-meta) any_dict)
                (term (store-lookup Σ ref_exn))))))


(define-syntax-rule (check-eval/str/chk program str global)
  (full-expect
   (,(python->redex program) ,basic-env ,basic-store)
   (side-condition
    ((pointer-val ref_str)
     ((x ref) (... ...) (chk ref_chk) (x_n ref_n) (... ...))
     (name Σ ((ref_1 val_1) (... ...)
              (ref_chk (pointer-val ref_chkval))
              (ref_rest val_rest) (... ...))))
    (and
     (redex-match λπ (obj-val any_cls (meta-num global) any_dict)
                  (term (store-lookup Σ ref_chkval)))
     (redex-match λπ (obj-val any_cls (meta-str str) {})
                  (term (store-lookup Σ ref_str)))))))

(check-eval/str/chk
 "
chk = 'return-in-finall1-chk-init'
def f():
  global chk
  try:
    return 'inside try'
  finally:
    chk = 10
    return 'inside finally'

f()
"
 "inside finally"
 10)
                  

(check-eval/str/chk
 "
chk = 'return-in-finally-chk-init'
def f():
  global chk
  try:
    return 'inside try'
  finally:
    chk = 10

f()
"
 "inside try"
 10)

(define return-in-while
   "
chk = 0
def f():
  global chk
  while True:
    chk = 100
    return 'inside while'

f()
")
(check-eval/str/chk return-in-while "inside while" 100)



(define return-in-while-in-finally
   "
chk = 'return-in-while-in-finally-chk-init'
def f():
  global chk
  try:
    while True:
      chk = 100
      return 'inside while'
  finally:
    if chk is 100:
      chk = 33

f()
")
(check-eval/str/chk return-in-while-in-finally "inside while" 33)

(define raise-in-while
  "
chk = 'raise-in-while-chk-init'
def f():
  global chk
  try:
    while True:
      chk = 100
      raise Exception()
  except:
    if chk is 100:
      chk = 33
    return 'inside except'

f()
")

(check-eval/str/chk raise-in-while "inside except" 33)

(define break-then-except-else
  "
chk = 'break-then-except-else-init'
try:
  while True:
    chk = 'in-loop'
    break
except:
  chk = 'better not be this'
  raise Exception()
else:
  if chk is 'in-loop':
    chk = 100
  result = 'made it'
result
")
(check-eval/str/chk break-then-except-else "made it" 100)

(define while-break-skips-else
  "
chk = 75
while True:
  break
else:
  chk = 100
'all done'
")
(check-eval/str/chk while-break-skips-else "all done" 75)

(define while-falls-to-else
    "
chk = 75
while False:
  pass
else:
  chk = 100
'all done'
")
(check-eval/str/chk while-falls-to-else "all done" 100)

(define while-continue-then-break
  "
chk = 'init'
while True:
  if chk is 'init':
    chk = 'not-init'
    continue
    chk = 'better-not-be-this'
  else:
    if chk is 'not-init':
      chk = 100
      break
    else:
      raise Exception()
'all done'
")
(check-eval/str/chk while-continue-then-break "all done" 100)


(define break-in-finally-in-while
  "
chk = 'init'
while True:
  try:
    chk = 'in loop'
    break
    chk = 'better-not-be-this'
  finally:
    if chk is 'in loop':
      chk = 100

'all done'
")
(check-eval/str/chk break-in-finally-in-while "all done" 100)

(define continue-in-finally-in-while
  "
chk = 'init'
while True:
  if chk is 'saw while':
    chk = 100
    break
  try:
    chk = 'in loop'
    continue
    chk = 'better-not-be-this'
  finally:
    if chk is 'in loop':
      chk = 'saw while'

'all done'
")
(check-eval/str/chk break-in-finally-in-while "all done" 100)


