#lang racket
(require (lib "eopl.ss" "eopl"))

(require "utils.rkt")
(require "scope_mem.rkt")

(define-datatype exp_val exp_val?
  (list_val (exprs list?))
  (bool_val (val boolean?))
  (null_val)
  (num_val (val number?))
  (proc_val (proc proc?))
  )

; for function definitions
(define-datatype proc proc?
  (new-proc
   (params eval-func-param*?)
   (statements list?)
   (parent-scope scope-index?)
   )
  )

(define-datatype eval-func-param eval-func-param?
  (eval_with_default (var symbol?) (val return-true))
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