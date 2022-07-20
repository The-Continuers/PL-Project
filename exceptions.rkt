#lang racket
(require (lib "eopl.ss" "eopl"))
(require "./utils.rkt")

(define-datatype exception exception?
    (new-exception (type symbol?) (message string?) (trace-back return-true))
)

(define (exception->exception-type e)
    (cases exception e
        (new-exception (type message trace-back) type)
    )
)

(define (exception->message e)
    (cases exception e
        (new-exception (type message trace-back) message)
    )
)

(define (exception->trace-back e)
    (cases exception e
        (new-exception (type message trace-back) trace-back)
    )
)

(define TypeError 'TypeError)

(define NameError 'NameError)

(define SyntaxError 'SyntaxError)


(define (raise e)
    (eopl:error 
        (exception->exception-type e) 
        (string-join (list (~v (exception->trace-back e)) (exception->message e)) "\n")
    )
)

(define (report-not-pair iter_list trace-back)
    (raise 
        (new-exception 
        TypeError 
        (string-join (list (~v iter_list) "object is not iterable" )) 
        trace-back
        )
    )
)

(define (report-not-boolean cond-val trace-back) 
    (raise 
        (new-exception 
        TypeError 
        (string-join (list (~v cond-val) "object is not a bool type") " ") 
        trace-back
        )
    )
)

(define (report-not-found trace-back var) 
    (raise 
        (new-exception 
        NameError 
        (string-join (list "name" (~v var) "is not defined" ) " ") 
        trace-back
        )
    )
)

(define (report-not-global trace-back var)
    (raise 
        (new-exception 
        SyntaxError 
        (string-join (list "name" (~v var) "is assigned to before global declaration" ) " ") 
        trace-back
        )
    )
)

(define (report-break-outside-loop trace-back)
    (raise 
        (new-exception 
        SyntaxError 
        "'break' outside loop" 
        trace-back
        )
    )
)

(define (report-continue-outside-loop trace-back) 
    (raise 
        (new-exception 
        SyntaxError 
        "'break' outside loop" 
        trace-back
        )
    )
)

(provide (all-defined-out))