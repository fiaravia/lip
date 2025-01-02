%{
open Ast
%}

%token TRUE
%token FALSE
%token LPAREN
%token RPAREN
%token IF
%token THEN
%token ELSE
%token AND
%token OR
%token SUCC
%token PRED
%token ISZERO
%token ZERO
%token EOF


%start <expr> prog

%%

prog:
  | e = expr; EOF { e }
;

expr:
  | TRUE { True }
  | FALSE { False }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr; { If(e1, e2, e3) }
  | LPAREN; e=expr; RPAREN {e}
  | a = expr; AND ;  b = expr; { If(a,b,False) }
  | a = expr; OR ;  b = expr; { If(a,True,b) }
  | ZERO { Zero }
  | SUCC; e = expr; { Succ(e) }
  | PRED; e = expr; { Pred(e) }
  | ISZERO; e = expr; { IsZero(e) }
  
;


