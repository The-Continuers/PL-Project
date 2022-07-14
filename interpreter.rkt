#lang racket
(require (lib "eopl.ss" "eopl"))


(require "parser.rkt")

(require "datatypes.rkt")
(#%require "datatypes.rkt")
(require "interpreter_data.rkt")
(#%require "interpreter_data.rkt")

; Scope
(define-datatype scope scope?
  (new-scope (env environment?) (parent-index scope-index?))
  )


(define (apply-scope scope search-var) #t)

(define (apply-scope-index scope-index search-var) #t)


(define (return-true x) #t)

; envs
(define-datatype environment environment?
  (init-env)
  (extend-env (var return-true) (val return-true) (before-env environment?))
  )

(define (apply-env env var1)
  (cases environment env
    (init-env () (report-no-binding-found var1))
    (extend-env (var val before-env) (if (equal? var var1) val (apply-env before-env var1)))
    )
  )


;interface
(define report-no-binding-found
  (lambda (search-var) (eopl:error 'apply-env "404: ~s" search-var))
  )


(define (apply-func proc1 param-values scope-index) #t)

(define (expression*->list-val expressions scope-index) #t)

(define (value-of expr scope-index)
  (cases expression expr
    (binary_op (op left right)
               (op (value-of left scope-index) (value-of right scope-index))
               )
    (unary_op (op operand) (op (value-of operand scope-index)))
    (function_call (func params)
                   (apply-func (value-of func scope-index)
                               (expression*->list-val params scope-index)
                               scope-index
                               ))
    (list_ref (ref index) (let ([ref-value (value-of ref scope-index)]
                                [index-value (value-of index scope-index)])
                            (list-ref ref index)
                            ))
    (ref (var) (apply-scope-index scope-index var))
    (atomic_bool_exp (bool) bool)
    (atomic_num_exp (num) num)
    (atomic_null_exp () null)
    (atomic_list_exp (l) (expression*->list-val l))
    )
  )


(define (extend-scope scope-index var expr-val) #t)

#|
  #1

a = 2;
global a

#2


def f():
	a = 4
	print(a)
	global a;
	print(a)

a = 2
|#

(define (check-global var scope-index) #t)

(define (extend-scope-globals scope-index var) #t)

(define (exec stmt scope-index)
  (cases statement stmt
    (assign (var expr) (let ([expr-val (value-of expr scope-index)]) (extend-scope scope-index var expr-val)))
    (global (var) (let ([checked (check-global var scope-index)]) (extend-scope-globals scope-index var)))
    (return (expr) (value-of expr scope-index))
    (return_void () null)
    (pass () null)
    (break () )
    )
  )

(define (exec-stmts stmts scope-index)
  (cond
    [(null? stmts) null]
    [(eq? (length stmts) 1) (exec (car stmts) scope-index)]
    [else (let ([first_exec (exec (car stmts) scope-index)])
            (exec-stmts (cdr stmts) scope-index))]
    )
  )