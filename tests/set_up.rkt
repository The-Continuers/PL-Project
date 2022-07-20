#lang racket

(require "../interpreter/scope/mem.rkt")
(require "../interpreter/scope/datatype.rkt")
(require "../interpreter/scope/procs.rkt")
(require "../interpreter/scope/env.rkt")
(#%require "../interpreter/scope/env.rkt")

(reset-scope)
(add-scope (init-scope))
(extend-scope-index ROOT "y" 2)
(extend-scope-index ROOT "l" (list 10 12 1 3 16))
(add-scope (new-scope (extend-env "y" 3 (init-env)) ROOT (list)))
(add-scope (new-scope (extend-env "x" 12 (init-env)) 1 (list "y")))