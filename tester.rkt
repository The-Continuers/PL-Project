#lang racket

(require "evaluate.rkt")

(define (evaluate-test i)
  (evaluate (string-append "test_cases/T" (~a i) "/in" (~a i) ".txt"))
  )