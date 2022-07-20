#lang racket
(require "../datatypes.rkt")

(define program-test-1 (list (func "fi" (func_params (with_default "x" (atomic_num_exp 0)) (empty-param))
                                   (list (if_stmt
                                          (binary_op (lambda (x y) (or x y))
                                                     (binary_op equal? (ref "x") (atomic_num_exp 0))
                                                     (binary_op equal? (ref "x") (atomic_num_exp 1))
                                                     )
                                          (return (atomic_num_exp 1))
                                          (return (binary_op +
                                                            (function_call (ref "fi")
                                                                            (expressions
                                                                             (binary_op - (ref "x") (atomic_num_exp 1))
                                                                             (empty-expr)))
                                                            (function_call (ref "fi")
                                                                            (expressions
                                                                             (binary_op - (ref "x") (atomic_num_exp 2))
                                                                             (empty-expr))
                                                                            ))))))
                             (return (function_call (ref "fi")
                                                    (expressions (atomic_num_exp 5) (empty-expr))))
                             ))

(define program-test-2 (list (func "gi" (func_params (with_default "x" (atomic_num_exp 0))
                                                     (func_params (with_default "y" (atomic_num_exp 1)) (empty-param)))
                                   (list (return (binary_op / (ref "x") (ref "y")))))
                             (return (function_call (ref "gi")
                                                    (expressions (atomic_num_exp 5)
                                                                 (expressions (atomic_num_exp 2) (empty-expr)))))
                             ))



(provide (all-defined-out))