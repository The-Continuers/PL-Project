#lang racket
(require (lib "eopl.ss" "eopl"))

(#%require "../datatypes.rkt")
(require "../exceptions.rkt")

; todo: checking for unusual breaks after parsing
(define (break_check_st st sts is-for)
  (cases statement st
    (break () (if is-for null (report-break-outside-loop sts)))
    (continue () (if is-for null (report-continue-outside-loop sts)))
    (func (name params f_sts) (break_check_sts f_sts #f))
    (if_stmt (cond_exp if_sts else_sts)
             (begin
               (break_check_sts if_sts is-for)
               (break_check_sts else_sts is-for)
               ))
    (for_stmt (iter list_exp for_sts)
              (break_check_sts for_sts #t))
    (else null)
    ))

(define (break_check_sts sts [is-for #f])
  (if (null? sts) null
      (begin
        (break_check_st (car sts) sts is-for)
        (break_check_sts (cdr sts) is-for)
        ))
  )

(define (break_checker prg)
  (begin
    (cases program prg
      (new-program (typed sts) (break_check_sts sts))
      )
    prg
    )
  )

(provide (all-defined-out))