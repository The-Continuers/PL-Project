#lang racket

(define (return-true x) #t)

(define (contains l x) (if (member x l) #t #f))

(provide (all-defined-out))