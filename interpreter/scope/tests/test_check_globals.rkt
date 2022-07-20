#lang racket
(require rackunit)

(require "../mem.rkt")
(require "../datatype.rkt")
(require "../procs.rkt")
(require "../env.rkt")
(#%require "../env.rkt")


(reset-scope)
(add-scope (init-scope))
(extend-scope-index ROOT "y" 1)
(add-scope (new-scope (extend-env "x" 12 (init-env)) 0 (list)))
(check-eq? (check-global "y" 1) '())

(add-scope (new-scope (extend-env "x" 12 (extend-env "y" 10 (init-env))) 0 (list)))
(check-exn exn:fail? (lambda () (check-global "y" 2)))
