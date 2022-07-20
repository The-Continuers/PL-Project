#lang racket

(require rackunit)
(require "../../tests/set_up_program.rkt")
(require "../../datatypes.rkt")
(#%require "../../datatypes.rkt")
(require "../executor.rkt")
(require "../scope/mem.rkt")

(check-eq? (exec-program program-test-2) 2.5)
