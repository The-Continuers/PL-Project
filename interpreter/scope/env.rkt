#lang racket
(require (lib "eopl.ss" "eopl"))

(require "../../utils.rkt")

; envs
(define-datatype environment environment?
  (init-env)
  (extend-env (var return-true) (val return-true) (before-env environment?))
  )

(define-datatype env-not-found env-not-found?
  (new-env-not-found)
  )

(define (apply-env env var1)
  (cases environment env
    (init-env () (new-env-not-found))
    (extend-env (var val before-env) (if (equal? var var1) val (apply-env before-env var1)))
    )
  )

(provide (all-defined-out))
(#%provide (all-defined))