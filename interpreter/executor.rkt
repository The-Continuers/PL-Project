#lang racket
(require (lib "eopl.ss" "eopl"))
(require racket/trace)

(require "../datatypes.rkt")
(#%require "../datatypes.rkt")

(require "interpreter_data.rkt")
(#%require "interpreter_data.rkt")

(require "scope/mem.rkt")
(require "scope/datatype.rkt")
(require "scope/procs.rkt")
(require "scope/env.rkt")

(require "../exceptions.rkt")

(require "../utils.rkt")

(define (eval-apply-scope-index scope-index var)
  (let ([thunk-val (apply-scope-index scope-index var)])
    (if (pythunk? thunk-val)
        (cases pythunk thunk-val
          (not-eval-thunk (expr i)
                          (let ([eval-expr (value-of expr i)])
                            (begin (extend-scope-index scope-index var (eval-thunk eval-expr))
                                   eval-expr
                                   )))
          (eval-thunk (val) val)
          (else thunk-val)
          )
        thunk-val
        )))

(define (extend-params-scopes -scope params param-exprs scope-index)
  (cases eval-func-param* params
    (empty-eval-func-param () -scope)
    (eval-func-params (eval-param rest-evals)
                      (cases eval-func-param eval-param
                        (eval_with_default
                         (param-var param-val)
                         (extend-params-scopes
                          (extend-scope -scope param-var
                                        (not-eval-thunk (expression*->first param-exprs) scope-index))
                          rest-evals (expression*->rest param-exprs) scope-index))
                        )
                      )
    ))

(define (apply-func proc1 param-exprs scope-index)
  (cases proc proc1
    (new-proc (params sts r_type parent-scope)
              (let ([func-scope (extend-params-scopes
                                 (new-scope (init-env) parent-scope (list))
                                 params param-exprs scope-index)])
                (exec-stmts sts (add-scope func-scope))))
    )
  )

(define (expression*->list-val -expressions scope-index)
  (cases expression* -expressions
    (empty-expr () (list))
    (expressions (expr rest-exprs)
                 (append (expression*->list-val rest-exprs scope-index) (list (value-of expr scope-index)))
                 )
    )
  )

(define (value-of expr scope-index)
  (cases expression expr
    (binary_op (op left right)
               (let ([left-value (value-of left scope-index)])
                 (if (and (eq? left-value 0) (eq? op *))
                     0
                     (op left-value (value-of right scope-index)))
                 ))
    (unary_op (op operand) (op (value-of operand scope-index)))
    (function_call (func params)
                   (apply-func (value-of func scope-index)
                               params
                               scope-index
                               ))
    (list_ref (ref index) (let ([ref-value (value-of ref scope-index)]
                                [index-value (value-of index scope-index)])
                            (list-ref ref-value index-value)
                            ))
    (ref (var) (eval-apply-scope-index scope-index var))
    (atomic_bool_exp (bool) bool)
    (atomic_num_exp (num) num)
    (atomic_null_exp () null)
    (atomic_list_exp (l) (expression*->list-val l scope-index))
    )
  )

(define (type-of expr scope-index)
  (cases expression expr
    (binary_op (op left right) (ex-unknown))
    (unary_op (op operand) (ex-unknown))
    (function_call (func params) (proc->r_type (value-of func scope-index)))
    (list_ref (ref index) (ex-int))
    (ref (var) (ex-int))
    (atomic_bool_exp (bool) (ex-bool))
    (atomic_num_exp (num) (ex-int))
    (atomic_null_exp () (ex-none))
    (atomic_list_exp (l) (ex-list))
    )
  )

(define (func-param->eval-func-param -func_param scope-index)
  (cases func_param -func_param
    (with_default (var expr) (eval_with_default (assignee-var->var var) (value-of expr scope-index)))
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


(define (apply-for iter iter_list sts scope-index parent_stmt)
  (begin
    ; (display-lines (list iter_list))
    (cond
      [(null? iter_list) null]
      [(not (pair? iter_list)) (report-not-pair iter_list parent_stmt)]
      [else (begin
              (extend-scope-index scope-index iter (car iter_list))
              ;  (display-lines (list (get-scope-by-index scope-index)))
              (let ([first_exec_result (exec-stmts sts scope-index)])
                (cond
                  [(equal? first_exec_result (new-break)) null]
                  [else (apply-for iter (cdr iter_list) sts scope-index parent_stmt)])
                )
              )]
      )))

(define (apply-if cond-val if-sts else-sts scope-index parent_stmt)
  (cond
    [(not (boolean? cond-val)) (report-not-boolean cond-val parent_stmt)]
    [cond-val (exec-stmts if-sts scope-index)]
    [else (exec-stmts else-sts scope-index)]
    )
  )

(define (check-safe-type var t ex_t)
  (if (or (equal? t (ex-unknown)) (equal? t ex_t))
      '()
      (report-not-right-type var ex_t)
      )
  )

(define (apply-assign var val-expr scope-index)
  (cases assignee-var var
    (typed-var (var t) (begin
                         (check-safe-type var (type-of val-expr scope-index) t)
                         (extend-scope-index scope-index var (not-eval-thunk val-expr scope-index))
                         ))
    (untyped-var (var) (extend-scope-index scope-index var (not-eval-thunk val-expr scope-index)))
    )
  )

(define (apply-print vals)
  ; (display-lines (list "printing"))
  (display-lines vals)
  )

(define (exec stmt scope-index)
  (cases statement stmt
    (assign (var expr) (apply-assign var expr scope-index))
    (global (var) (let ([checked (check-global var scope-index)]) (extend-scope-index-globals scope-index var)))
    (return (expr) (value-of expr scope-index))
    (return_void () null)
    (pass () null)
    (break () (new-break))
    (continue () (new-continue))
    (func (name params statements r_type) (
                                           let ([
                                                 prc (new-proc
                                                      (func_params->eval-func-params params scope-index)
                                                      statements
                                                      r_type
                                                      scope-index)
                                                     ])
                                            (extend-scope-index scope-index name prc)
                                            ))
    (if_stmt (cond_exp if_sts else_sts) (apply-if
                                         (value-of cond_exp scope-index) if_sts else_sts scope-index
                                         stmt))
    (for_stmt (iter list_exp sts) (apply-for
                                   iter (value-of list_exp scope-index) sts scope-index
                                   stmt))
    (print_stmt (expressions) (apply-print (expression*->list-val expressions scope-index)))
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

(define (exec-program prg)
  (begin
    (reset-scope)
    (add-scope (init-scope))
    (cases program prg
      (new-program (typed sts) (exec-stmts sts ROOT))
      )
    (void)
    )
  )

; (trace exec-stmts exec value-of type-of apply-assign check-safe-type)

(provide (all-defined-out))