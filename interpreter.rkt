#lang racket
(require (lib "eopl.ss" "eopl"))


(require "parser.rkt")

(require "datatypes.rkt")
(#%require "datatypes.rkt")
(require "interpreter_data.rkt")
(#%require "interpreter_data.rkt")

(require "exceptions.rkt")

(define (apply-func proc1 param-values scope-index) #t)

(define (expression*->list-val -expressions scope-index)
  (cases expression* -expressions
    (empty-expr () (list))
    (expressions (expr rest-exprs) 
      (cons (value-of expr scope-index) (expression*->list-val rest-exprs scope-index))
    )
  )
)

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


(define (apply-for iter iter_list sts scope-index parent_stmt)
  (cond
    [(not (pair? iter)) (report-not-pair iter_list parent_stmt)]
    [(null? iter_list) null]
    [else (let ([_ (extend-scope scope-index iter (car iter_list))])
            (let ([first_exec_result (exec-stmts sts scope-index)])
              (cond
                [(equal? first_exec_result (new-break)) null]
                [else (apply-for iter (cdr iter_list) sts scope-index parent_stmt)])
              )
            )]
    ))

(define (func-param->eval-func-param -func_param scope-index)
  (cases func_param -func_param
    (with_default (var expr) (eval_with_default var (value-of expr scope-index)))
  )
)

(define (func_params->eval-func-params -func_params scope-index)
  (cases func_param* -func_params
    (empty-param () (empty-eval-func-param))
    (func_params (param rest-params) 
      (eval-func-params 
        (func-param->eval-func-param param scope-index)
        (func_params->eval-func-params rest-params scope-index)
      )
    )
  )
)

(define (apply-if cond-val if-sts else-sts scope-index parent_stmt)
  (cond
    [(boolean? cond-val) (report-not-boolean cond-val parent_stmt)]
    [cond-val (exec-stmts if-sts scope-index)]
    [else (exec-stmts else-sts scope-index)]
    )
)

(define (exec stmt scope-index)
  (cases statement stmt
    (assign (var expr) (let ([expr-val (value-of expr scope-index)]) (extend-scope scope-index var expr-val)))
    (global (var) (let ([checked (check-global var scope-index)]) (extend-scope-globals scope-index var)))
    (return (expr) (value-of expr scope-index))
    (return_void () null)
    (pass () null)
    (break () (new-break))
    (continue () (new-continue))
    (func (name params statements) (
      let ([prc (new-proc (func_params->eval-func-params params scope-index) statements scope-index)])
      (extend-scope scope-index name prc)
    ))
    (if_stmt (cond_exp if_sts else_sts) (apply-if
                                         (value-of cond_exp scope-index) if_sts else_sts scope-index
                                         stmt))
    (for_stmt (iter list_exp sts) (apply-for
                                   iter (value-of list_exp scope-index) sts scope-index
                                   stmt))
    )
  )

(define (exec-stmts stmts scope-index)
  (cond
    [(null? stmts) null]
    [(eq? (length stmts) 1) (exec (car stmts) scope-index)]
    [else (let ([first_exec (exec (car stmts) scope-index)])
            (if (or
                 (equal? first_exec (new-break))
                 (equal? first_exec (new-continue)))
                first_exec
                (exec-stmts (cdr stmts) scope-index))
            )
          ]
    ))
