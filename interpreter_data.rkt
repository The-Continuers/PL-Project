#lang racket
(require (lib "eopl.ss" "eopl"))


(define scope-mem (list))

(define (return-true x) #t)

(define-datatype exp_val exp_val?
  (list_val (exprs list?))
  (bool_val (val boolean?))
  (null_val)
  (num_val (val number?))
  (proc_val (proc proc?))
  )

(define scope-index? (lambda (n) (< n (length scope-mem))))

; for function definitions
(define-datatype proc proc?
  (new-proc
   (params list?)
   (statements list?)
   (parent-scope scope-index?)
   )
  )

(define-datatype eval-func-param eval-func-param?
  (eval_with_default (var symbol?) (expr return-true))
  )

(define-datatype eval-func-param* eval-func-param*?
  (empty-eval-func-param)
  (eval-func-params (eval-param eval-func-param?) (rest-evals eval-func-param*?))
  )

; for function calls
(define-datatype activation-record activation-record?
  (new-record (proc proc?) (my-scope scope-index?))
  )

(define-datatype for-signal for-signal?
  (new-break)
  (new-continue)
  )

(provide (all-defined-out))
(#%provide (all-defined))