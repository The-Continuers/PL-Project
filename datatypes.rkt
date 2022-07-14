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

(define-datatype func_param func_param?
  (with_default (var symbol?) (expr expression?))
  )

(define-datatype expression expression?
  (binary_op (op procedure?) (left expression?) (right expression?))
  (unary_op (op procedure?) (operand expression?))
  (function_call (func expression?) (params list?))
  (list_ref (ref expression?) (index expression?))
  (atomic_exp (expr exp_val?))
  (ref (var symbol?))
  )
(define-datatype exp_val exp_val?
  (list_val (exprs list?))
  (bool_val (val boolean?))
  (null_val)
  (num-val (val number?))
  
  )



;todo init_structure "Team"

(provide (all-defined-out))
(#%provide (all-defined))