#lang racket
(require (lib "eopl.ss" "eopl"))

(require "../utils.rkt")
(require "scope/mem.rkt")
(require "../datatypes.rkt")


; for function definitions
(define-datatype proc proc?
  (new-proc
   (params eval-func-param*?)
   (statements list?)
   (parent-scope scope-index?)
   )
  )

(define-datatype eval-func-param eval-func-param?
  (eval_with_default (var string?) (val return-true))
  )

(define-datatype eval-func-param* eval-func-param*?
  (empty-eval-func-param)
  (eval-func-params (eval-param eval-func-param?) (rest-evals eval-func-param*?))
  )

(define-datatype for-signal for-signal?
  (new-break)
  (new-continue)
  )

(define-datatype pythunk pythunk?
  (not-eval-thunk (expr expression?) (i scope-index?))
  (eval-thunk (val return-true))
  )

(provide (all-defined-out))
(#%provide (all-defined))