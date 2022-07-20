#lang racket

(require "main.rkt")

(interpret (string-join (file->lines "test_cases/T4 (General Test) - Students/in4.txt") "\n"))

