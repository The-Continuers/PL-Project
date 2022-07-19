#lang racket
(require (lib "eopl.ss" "eopl"))

(require "env.rkt")
(#%require "env.rkt")

(require "scope_mem.rkt")

; Datatype
(define-datatype scope scope?
  (new-scope (env environment?) (parent-index scope-index?) (globals list?))
  )

(define (scope->env -scope)
  (cases scope -scope
    (new-scope (env parent-index globals) env)
    ))

(define (scope->parent-index -scope)
  (cases scope -scope
    (new-scope (env parent-index globals) parent-index)
    ))

(define (scope->globals -scope)
  (cases scope -scope
    (new-scope (env parent-index globals) globals)
    ))


(add-scope (new-scope (init-env) -1 '()))

(provide (all-defined-out))
(#%provide (all-defined))