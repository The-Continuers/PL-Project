#lang racket


(require rackunit)
(require "../../tests/set_up.rkt")
(require "../../datatypes.rkt")
(#%require "../../datatypes.rkt")
(require "../executor.rkt")
(require "../scope/mem.rkt")

(check-eq? (value-of (function_call (function_call (ref "f") (empty-expr)) (empty-expr)) ROOT) 8)
