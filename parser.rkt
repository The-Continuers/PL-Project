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
    (Param_with_default ((ID ASSIGN Expression) (with_default (token-value $1) $3)))
    (If_stmt ((IF Expression COLON Statements Else_block)(if_stmt $2 $4 $5)))
    (Else_block ((ELSE COLON Statements)($3)))
    (For_stmt ((FOR ID IN Expression COLON Statements)((for_stmt (token-value $2) $4 $6))))
    (Expression ((Disjunction)($1)))
    (Disjunction ((Conjunction)($1)) ((Disjunction OR Conjunction)(binary_op or $1 $3)))
    (Conjunction ((Inversion)($1))((Conjunction AND Inversion)(binary_op and $1 $3)))
    (Inversion ((NOT Inversion)(unary_op not $2)) ((Comparison)($1)))
    (Comparison ((Eq_Sum)($1)) ((Lt_Sum)($1)) ((Gt_Sum)($1)) ((Sum)($1)))
    (Eq_Sum ((Sum ISEQ Sum)(binary_op equal? $1 $3)))
    (Lt_Sum ((Sum LT Sum)(binary_op < $1 $3)))
    (Gt_Sum ((Sum BT Sum)(binary_op > $1 $3)))
    (Sum ((Sum PLUS Term)(binary_op + $1 $3))
     ((Sum MINUS Term)(binary_op - $1 $3)) 
     ((Term)($1)))
    (Term ((Term MULTI Factor)(binary_op * $1 $3))((Term DIV Factor)(binary_op / $1 $3))((Factor)($1)))
    (Factor ((PLUS Power)(unary_op + $2))((MINUS Power)(unary_op - $2))((Power)($1)))
    (Power ((Atom POW Factor)(binary_op (lambda (x y) (expt x y)) $1 $3))((Primary)($1)))
    (Primary ((Atom)($1))((Primary LBRACK Expression RBRACK)(list_ref $1 $3))((Primary LPAR RPAR)(function_call $1 (list)))((Primary LPAR Arguments RPAR)(function_call $1 $3)))
    (Arguments ((Expression)($1))((Arguments COMMA Expression)(append $1 (list $3))))
    (Atom ((ID)(token-value $1)))

    
   ))

;test
(define lex-this2 (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer2 (lex-this2 python-lexer (open-input-string "1+2+ 3 +   4")))
(let ((parser-res (python-parser my-lexer2))) parser-res)

(provide (all-defined-out))