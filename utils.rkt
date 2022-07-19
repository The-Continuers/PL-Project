#lang racket

(define (return-true x) #t)

(define (contains l x) (not (null? (member x l))))

(provide (all-defined-out))