#lang racket

(require "main.rkt")

(define test-1 (string-join (file->lines "test_cases/T4 (General Test) - Students/in4.txt")))

(interpret test-1)
