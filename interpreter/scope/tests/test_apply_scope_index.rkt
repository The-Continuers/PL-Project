#lang racket
(require rackunit)

(require "../mem.rkt")
(require "../datatype.rkt")
(require "../procs.rkt")
(require "../env.rkt")
(#%require "../env.rkt")


(reset-scope)
(add-scope (init-scope))
(extend-scope-index ROOT "y" 100)
(add-scope (new-scope (extend-env "y" 12 (init-env)) ROOT (list)))
(add-scope (new-scope (extend-env "x" 12 (init-env)) 1 (list "y")))
(check-true (is-defined-as-global? (get-scope-by-index 2) "y"))
(check-eq? (apply-scope-index 2 "y") 100)
