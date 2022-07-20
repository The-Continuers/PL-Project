#lang racket
(require (lib "eopl.ss" "eopl"))

(require "mem.rkt")

(require "datatype.rkt")
(#%require "datatype.rkt")

(require "env.rkt")
(require "../../utils.rkt")
(require "../../exceptions.rkt")


(define (apply-scope-index scope-index var)
  (
   let ([my-scope (get-scope-by-index scope-index)])
    (
     (
      let ([res (apply-env (scope->env my-scope) var)])
       (cond
         [(not (equal? res (new-env-not-found))) res]
         [(>= (scope->parent-index my-scope) 0) (apply-scope-index (scope->parent-index my-scope))]
         [else (report-not-found my-scope var)]
         )
       )
     )
    )
  )

(define (init-scope) (new-scope (init-env) -1 '()))

(define (extend-scope -scope -var -val)
  (let ([new-env (extend-env -var -val (scope->env -scope))])
    (new-scope new-env
               (scope->parent-index -scope)
               (scope->globals -scope))
    ))

(define (extend-scope-index scope-index var val)
  (set-scope scope-index
             (extend-scope (get-scope-by-index scope-index) var val)
             ))

(define (extend-scope-globals -scope var)
  (new-scope (scope->env -scope) (scope->parent-index -scope)
             (append (scope->globals -scope) (list var))
             ))

(define (extend-scope-index-globals scope-index var)
  (set-scope scope-index
             (extend-scope-globals (get-scope-by-index scope-index) var)
             ))


(define (scope-index-contains-itself var scope-index )
  (not (equal?
        (apply-env (scope->env (get-scope-by-index scope-index)) var)
        (new-env-not-found))))

(define (is-global? var scope-index)
  (contains (scope->globals (get-scope-by-index scope-index)) var)
  )

(define (check-global var scope-index)
  (if (scope-index-contains-itself var scope-index)
      (report-not-global (get-scope-by-index scope-index) var)
      '()))


(provide (all-defined-out))