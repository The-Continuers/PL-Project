#lang racket

(require "main.rkt")

(define test-1 (string-join (file->lines "test_cases/T4 (General Test) - Students/in4.txt")))

; test-1
(display-lines (list test-1))

(interpret (string-join (file->lines "test_cases/T4 (General Test) - Students/in4.txt")))
