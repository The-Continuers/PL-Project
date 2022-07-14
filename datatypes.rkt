#lang racket
(require (lib "eopl.ss" "eopl"))

(define-datatype statement statement?
  (assign (var symbol?) (expr expression?))
  (global (var symbol?))
  (return (expr expression?))
  (return_void)
  (pass)
  (break)
  (continue)
  (func (name symbol?) (params list?) (statements list?))
  (if_stmt (cond_exp expression?) (if_sts list?) (else_sts list?))
  (for_stmt (iter symbol?) (list_exp expression?) (sts list?))
  )

(define-datatype func_params func_params?
  ()
  )

(define-datatype expression expression?
  ()
  )

;todo init_structure "Team"

(provide (all-defined-out))
(#%provide (all-defined))