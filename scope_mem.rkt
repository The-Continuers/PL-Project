#lang racket

; Data Memory
(define scope-mem (list))

(define scope-index? (lambda (n) (< n (length scope-mem))))

(define (get-scope-by-index scope-index)
  (list-ref scope-mem scope-index)
  )

(define (set-scope scope-index -scope)
  (set! scope-mem
        (list-set scope-mem scope-index -scope)
        ))

(define (add-scope -scope)
  (begin
    (set! scope-mem (append scope-mem (list -scope)))
    (- (length scope-mem) 1)
    ))

(provide (all-defined-out))