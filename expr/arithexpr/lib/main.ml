open Ast

let unBool = function
    Bool b -> b
  | _ -> failwith "Expected a boolean value"

let unNat = function
    Nat n -> n
  | _ -> failwith "Expected a natural number"

let rec is_nv = 


let rec string_of_expr: expr -> string = function
    True -> "True"
  | False -> "False"
  | If(e0,e1,e2) -> "If(" ^ (string_of_expr e0) ^ "," ^ (string_of_expr e1) ^ "," ^ (string_of_expr e2) ^ ")"
  | Zero -> "Zero"
  | Succ(e) -> "Succ(" ^ (string_of_expr e) ^ ")"
  | Pred(e) -> "Pred(" ^ (string_of_expr e) ^ ")"
  | IsZero(e) -> "IsZero(" ^ (string_of_expr e) ^ ")"
  | _ -> failwith "Invalid argument to string_of_expr"
  


let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


exception NoRuleApplies

let rec trace1 = function
    If(True,e1,_) -> e1
  | If(False,_,e2) -> e2
  | If(cond,e1,e2) -> If(trace1 cond, e1, e2)
  | _ -> raise NoRuleApplies

let rec trace e = try
    let e' = trace1 e
    in e::(trace e')
  with NoRuleApplies -> [e]


let rec eval: expr -> exprval = function
    True -> Bool true
  | False -> Bool false
  | If(cond, e1, e2) ->
   ( match unBool( eval cond )with
    | true -> eval e1
    | false -> eval e2)
  | Zero -> Nat 0
  | Succ(e) -> Nat (unNat(eval e) + 1 )
  | Pred(e) when  unNat (eval e) > 0  -> Nat (unNat(eval e) - 1) 
  | IsZero(e) -> if unNat(eval e) = 0 then Bool true else (if(unNat(eval e) > 0) then Bool false else failwith "Invalid argument to eval")
  | _ -> failwith "Invalid argument to eval"

  