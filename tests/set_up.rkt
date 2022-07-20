#lang racket

(require "../interpreter/scope/mem.rkt")
(require "../interpreter/scope/datatype.rkt")
(require "../interpreter/scope/procs.rkt")
(require "../interpreter/scope/env.rkt")
(#%require "../interpreter/scope/env.rkt")

(require "../interpreter/interpreter_data.rkt")
(#%require "../interpreter/interpreter_data.rkt")

(require "../datatypes.rkt")
(#%require "../datatypes.rkt")

(reset-scope)
(add-scope (init-scope))
(extend-scope-index ROOT "y" 2)
(extend-scope-index ROOT "l" (list 10 12 1 3 16))
(extend-scope-index ROOT "f" (new-proc (empty-eval-func-param)
                                       (list (assign "x" (atomic_num_exp 3))
                                             (func "g" (empty-param) (list
                                                                      (return (binary_op * (ref "x") (atomic_num_exp 2)))
                                                                      ))
                                             (assign "x" (atomic_num_exp 4))
                                             (return (function_call (ref "g") (empty-expr)))
                                             )))
(add-scope (new-scope (extend-env "y" 3 (init-env)) ROOT (list)))
(add-scope (new-scope (extend-env "x" 12 (init-env)) 1 (list "y")))