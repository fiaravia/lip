{
open Parser
}

let white = [' ' '\t']+

rule read =
  parse
  | white { read lexbuf }  
  | "true" { TRUE }
  | "false" { FALSE }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "&&" { AND }
  | "||" { OR }
  | "succ" { SUCC }
  | "pred" { PRED }
  | "iszero" { ISZERO }
  | "0" { ZERO }
  | eof { EOF }

