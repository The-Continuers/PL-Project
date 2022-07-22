#lang racket

(define (return-true x) #t)

(define (contains l x) (if (member x l) #t #f))

(define (display-return l) (begin (display-lines (list l)) l ))

(provide (all-defined-out))