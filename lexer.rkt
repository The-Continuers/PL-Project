#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens LITERALS (ID NUMBER))

(define-empty-tokens END (EOF))
(define-empty-tokens KWS (DEF GLOBAL PASS BREAK CONTINUE RETURN NONE))
(define-empty-tokens COND_KWS (IF ELSE))
(define-empty-tokens LOOP_KWS (FOR IN))
(define-empty-tokens BOOL_KWS (TRUE FALSE))
(define-empty-tokens OPS (ASSIGN LPAR RPAR PAR COLON PAR_COLON COMMA SEMICOLON))
(define-empty-tokens BOOL_OPS (OR AND NOT))
(define-empty-tokens COND_OPS (ISEQ LT BT))
(define-empty-tokens ARITH_OPS (PLUS MINUS MULTI DIV POW))
(define-empty-tokens INDEX_OPS (LBRACK RBRACK BRACK))

(define python-lexer
  (lexer
   ((:or (:+ (char-range #\0 #\9))
         (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))
    (token-NUMBER (string->number lexeme)))
   ("+" (token-PLUS))
   (whitespace (python-lexer input-port))
   ((eof) (token-EOF))))


;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this python-lexer (open-input-string "1+2+ 3 +   4")))
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)

(provide (all-defined-out))