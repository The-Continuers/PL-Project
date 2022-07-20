#lang racket

(require rackunit)
(require "../../tests/set_up.rkt")
(require "../../datatypes.rkt")
(#%require "../../datatypes.rkt")
(require "../executor.rkt")
(require "../scope/mem.rkt")

(define expr 
    (unary_op - 
        (binary_op * 
            (list_ref (ref "l") (atomic_num_exp 1)) 
            (binary_op + 
                (list_ref (ref "l") (ref "y")) 
                (atomic_num_exp 4)
            )
        )
    )
)

(check-eq? (value-of expr ROOT) -60)
(check-eq? (value-of expr 1) -84)