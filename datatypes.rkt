#lang racket
(require (lib "eopl.ss" "eopl"))

(define-datatype program program?
  (new-program (typed boolean?) (sts list?))
  )

(define-datatype statement statement?
  (assign (var assignee-var?) (expr expression?))
  (global (var string?))
  (return (expr expression?))
  (return_void)
  (pass)
  (break)
  (continue)
  (func (name string?) (params func_param*?) (statements list?) (r_type is-type?))
  (if_stmt (cond_exp expression?) (if_sts list?) (else_sts list?))
  (for_stmt (iter string?) (list_exp expression?) (sts list?))
  (print_stmt (expressions expression*?))
  )

(define-datatype ex-type is-type?
  (ex-int)
  (ex-bool)
  (ex-float)
  (ex-list)
  (ex-none)
  (ex-unknown)
  )

(define-datatype assignee-var assignee-var?
  (typed-var (var string?) (type is-type?))
  (untyped-var (var string?))
  )

(define (assignee-var->var ass-var)
  (cases assignee-var ass-var
    (typed-var (var type) var)
    (untyped-var (var) var)
    )
  )

(define-datatype func_param func_param?
  (with_default (var assignee-var?) (expr expression?))
  )

(define-datatype func_param* func_param*?
  (empty-param)
  (func_params (param func_param?) (rest-params func_param*?))
  )

(define-datatype expression expression?
  (binary_op (op procedure?) (left expression?) (right expression?))
  (unary_op (op procedure?) (operand expression?))
  (function_call (func expression?) (params expression*?))
  (list_ref (ref expression?) (index expression?))
  (ref (var string?))

  (atomic_bool_exp (bool boolean?))
  (atomic_num_exp (num number?))
  (atomic_null_exp)
  (atomic_list_exp (l expression*?))
  )

(define-datatype expression* expression*?
  (empty-expr)
  (expressions (expr expression?) (rest-exprs expression*?))
  )

(define (expression*->first exprs)
  (cases expression* exprs
    (expressions (expr rest-exprs) expr)
    (else null)
    )
  )

(define (expression*->rest exprs)
  (cases expression* exprs
    (expressions (expr rest-exprs) rest-exprs)
    (else null)
    )
  )


;todo init_structure "Team"

(provide (all-defined-out))
(#%provide (all-defined))