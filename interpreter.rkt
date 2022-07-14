#lang racket


(require "parser.rkt")

(require "datatypes.rkt")
(#%require "datatypes.rkt")
(require "interpreter_data.rkt")
(#%require "interpreter_data.rkt")



; Scope
;grammar

(define-datatype scope scope?
  (empty-scope)
  (extend-scope (previous-scope scope?) (key (lambda (x) #t)) (value (lambda (x) #t)))
)


;interface
(define report-no-binding-found
  (lambda (search-var) (eopl:error 'apply-env "404: ~s" search-var))
 )


(define apply-env
  (
   lambda (env search-var)
    (
     cases scope env
      (empty-scope () (report-no-binding-found search-var))
      (
       extend-scope (previous-scope key value)
        (if (eqv? key search-var) value (apply-env previous-scope search-var))
       )
     )
   )
  )
