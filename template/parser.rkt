#lang racket


(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define simple-math-lexer
  (lexer
   ((:or (:+ (char-range #\0 #\9))
         (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9))))
    (token-NUM (string->number lexeme)))
   ("+" (token-plus))
   (whitespace (simple-math-lexer input-port))
   ((eof) (token-EOF))))

(define-tokens a (NUM))
(define-empty-tokens b (EOF plus))

(define simple-math-parser
  (parser
   (start exp)
   (end EOF)
   (error void)
   (tokens a b)
   (grammar
    (exp ((exp plus NUM) (list 'plusnumbers $1 $3))
         ((NUM) (list 'anumber $1))))))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "1+2+ 3 +   4")))
(let ((parser-res (simple-math-parser my-lexer))) parser-res)

