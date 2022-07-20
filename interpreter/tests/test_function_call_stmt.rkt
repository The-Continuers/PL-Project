#lang racket

(require rackunit)
(require "../../tests/set_up_program.rkt")
(require "../../datatypes.rkt")
(#%require "../../datatypes.rkt")
(require "../executor.rkt")
(require "../scope/mem.rkt")

(check-equal? (exec-program program-test-2) 5/2)

(check-equal? (exec-program program-test-1) 8)