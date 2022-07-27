#lang racket

(require "main.rkt")

(define (evaluate file-name)
  (interpret (string-join (file->lines file-name)))
  )

(provide (all-defined-out))