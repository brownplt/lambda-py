#lang plai-typed

(require  "../../python-core-syntax.rkt"
          "define-module.rkt")

(define-module imp-module
  '$imp
  ('module_finder (CNone))
  ('module_loader (CNone)))
