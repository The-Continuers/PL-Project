#lang racket

(require "lexer.rkt")
(require parser-tools/lex
         parser-tools/yacc)

(require "datatypes.rkt")
(#%require "datatypes.rkt")

(define python-parser
  (parser
   (start Program)
   (end EOF)
   (error void)
   (tokens
    LITERALS KWS OPS LOOP_KWS BOOL_KWS BOOL_OPS
    COND_KWS COND_OPS ARITH_OPS INDEX_OPS END
    )
   (grammar
    ;todo init_structure "ashk" || "sad"
    ;todo init_tree_creation "Team"
    (exp ((exp PLUS NUMBER) (list 'plusnumbers $1 $3))
         ((NUMBER) (list 'anumber $1)))
    (Program ((Statements) $1))
    (Statements ((Statement SEMICOLON) (list $1))
                ((Statements Statement SEMICOLON) (append $1 (list $2))))
    (Statement ((Compound_stmt) $1)
               ((Simple_stmt) $1))
    (Simple_stmt ((Assignment) $1)
                 ((Global_stmt) $1)
                 ((Return_stmt) $1)
                 ((PASS) (pass))
                 ((BREAK) (break))
                 ((CONTINUE) (continue)))
    (Compound_stmt ((Function_def) $1)
                   ((If_stmt) $1)
                   ((For_stmt) $1)
                   )
    (Assignment ((ID ASSIGN Expression) (assign (token-value $1) $2)))
    (Return_stmt ((RETURN) (return_void))
                 ((RETURN Expression) (return $2))
                 )
    (Global_stmt (GLOBAL ID) (global (token-value $2)))
    (Function_def ((DEF ID LPAR Params RPAR COLON Statements) (func (token-value $2) $4 $7))
                  ((DEF ID PAR_COLON Statements) (func (token-value $2) (list) $4))
                  )
    (Params ((Param_with_default) (list $1))
            ((Params COMMA Param_with_default) (append $1 (list $3)))
            )
    (Param_with_default ((ID ASSIGN Expression) ()))
    )
   ))

;test
(define lex-this2 (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer2 (lex-this2 python-lexer (open-input-string "1+2+ 3 +   4")))
(let ((parser-res (python-parser my-lexer2))) parser-res)

(provide (all-defined-out))