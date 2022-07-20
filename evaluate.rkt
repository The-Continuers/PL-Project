#lang racket

(require "main.rkt")

(define (evaluate file-name)
  (interpret (string-join (file->lines file-name)))
  )

(evaluate "test_cases/T4 (General Test) - Students/in4.txt")
