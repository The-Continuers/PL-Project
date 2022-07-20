#lang racket

(define (report-not-pair iter_list parent_stmt) #t)

(define (report-not-boolean cond-val parent_stmt) #t)

(define (report-not-found scope var) #t)

(define (report-not-global -scope var) #t)

(define (report-break-outside-loop sts) #t)

(define (report-continue-outside-loop sts) #t)

(provide (all-defined-out))