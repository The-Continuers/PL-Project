#lang racket
(require (lib "eopl.ss" "eopl"))

(require "exceptions.rkt")


(define scope-mem (list (new-scope (init-env) -1 (list))))

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

; Scope
(define-datatype scope scope?
  (new-scope (env environment?) (parent-index scope-index?) (globals list?))
  )

(define (scope->env -scope)
  (cases scope -scope
    (new-scope (env parent-index globals) env)
    )
  )

(define (scope->parent-index -scope)
  (cases scope -scope
    (new-scope (env parent-index globals) parent-index)
    )
  )

(define (scope->globals -scope)
  (cases scope -scope
    (new-scope (env parent-index globals) globals)
    )
  )

(define (get-scope-by-index scope-index)
  (list-ref scope-mem scope-index)
  )

(define (extend-scope -scope -var -car)
  (let ([new-env (extend-env var val (scope->env -scope))])
    (new-scope new-env
               (scope->parent-index -scope)
               (scope->globals -scope))
    )
  )

(define (set-scope scope-index -scope)
  (set! scope-mem
        (list-set scope-mem scope-index -scope)
        ))

(define (extend-scope-index scope-index var val)
  (set-scope scope-index
             (extend-scope (get-scope-by-index scope-index) var val)
             ))

(define (apply-scope-index scope-index var)
  (
   let ([my-scope (get-scope-by-index scope-index)])
    (
     (
      let ([res (apply-env (scope->env my-scope))])
       (cond
         [(not (equal? res (new-env-not-found))) res]
         [(>= (scope->parent-index my-scope) 0) (apply-scope-index (scope->parent-index my-scope))]
         [else (report-not-found my-scope var)]
         )
       )
     )
    )
  )

(define (extend-scope-globals -scope var)
  (new-scope (scope->env -scope) (scope->parent-index -scope)
             (append (scope->globals -scope) (list var))
             ))

(define (extend-scope-index-globals scope-index var)
  (set-scope scope-index
             (extend-scope-globals (get-scope-by-index scope-index) var val)
             ))

(define scope-index? (lambda (n) (< n (length scope-mem))))

(define (return-true x) #t)

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