#lang racket
(require (lib "eopl.ss" "eopl"))


(define scope-mem (list))


(define-datatype exp_val exp_val?
  (list_val (exprs list?))
  (bool_val (val boolean?))
  (null_val)
  (num_val (val number?))
  (proc_val (proc proc?))
  )

(define scope-index? (lambda (n) (< n (length scope-mem))))

  (define-datatype proc proc?
  (procedure
   (params list?)
   (statements list?)
   (parent-scope scope-index?)
   )
  )