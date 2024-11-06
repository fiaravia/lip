{
  open Parser
}

let white = [' ' '\t']+
let num = ['0'-'9']|['1'-'9']['0'-'9']* | "0x" ['0'-'9' 'a'-'f' 'A'-'F']* | "0b" ['0'-'1']+ | "0o" ['0'-'7']+ 


rule read_token =
  parse
  | white { read_token lexbuf }  
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { MULTIPLY }
  | "/" { DIVIDE }
  | num { CONST (Lexing.lexeme lexbuf) }
  | eof { EOF }
