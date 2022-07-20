#lang racket

(require "passes/parser.rkt")
(require "passes/break_checker.rkt")
(require "executor.rkt")

(define interpret (compose exec-program break_checker parse-scan))

(provide (all-defined-out))