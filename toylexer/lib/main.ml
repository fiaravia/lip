open Token
    
(* tokenize : Lexing.lexbuf -> LexingLib.Token.token list *)

let rec tokenize lexbuf =
  match Lexer.read_token lexbuf with
    EOF -> [EOF]
  | t -> t::(tokenize lexbuf)

(* lexer : string -> LexingLib.Token.token list *)

let lexer (s : string) =
  let lexbuf = Lexing.from_string s in
  tokenize lexbuf

(* string_of_tokenlist : token list -> string *)
    
let string_of_tokenlist tl = 
  List.fold_left (fun s t -> s ^ (string_of_token t ^ (if t=EOF then "" else " "))) "" tl

(* string_of_frequencies : (token * int) list -> string *)
    
let string_of_frequencies fl =
  List.fold_left (fun s (t,n) -> s ^ ((string_of_token t) ^ " -> " ^ string_of_int n ^ "\n")) "" fl

(* frequency : int -> 'a list -> ('a * int) list *)

(*
    frequency should output the n most occurrences of tokens in the list, in descending  
    order and return a list of pairs of token and token count
*)

(*
type token =
  | LPAREN
  | RPAREN
  | ASSIGN
  | PLUS
  | SEQ
  | ID of string
  | CONST of string
  | EOF
*)

let compare_token t1 t2 = 
  match t1, t2 with
    (a, b) when a == b -> 0
  | (CONST(_), CONST(_)) -> 0 
  | (ID(a), ID(b)) -> if a > b then 1 else -1
  | (ID(_), _) -> 1
  | (_, ID(_)) -> -1 
  | (CONST(_), _) -> 1
  | (_, CONST(_)) -> -1 
  | (LPAREN, _) -> 1
  | (_, LPAREN) -> -1 
  | (RPAREN, _) -> 1
  | (_, RPAREN) -> -1 
  | (ASSIGN, _) -> 1
  | (_, ASSIGN) -> -1 
  | (PLUS, _) -> 1
  | (_, PLUS) -> -1 
  | (SEQ, _) -> 1
  | (_, SEQ) -> -1
  | _ -> 0
  


(*sort elements*)
let sort_elements l = List.sort compare_token l ;;

let lista_token =  [ID("x"); ASSIGN; ID("y"); SEQ; ID("x"); ASSIGN; ID("x"); PLUS; CONST("1")];;

sort_elements lista_token;;

let count_elements lst =
  let rec aux current count acc = function
    | [] -> (current, count) :: acc
    | x :: xs ->
      if x = current then
        aux current (count + 1) acc xs
      else
        aux x 1 ((current, count) :: acc) xs
  in
  match lst with
  | [] -> []
  | x :: xs -> List.rev (aux x 1 [] xs);;

let sort_descending lst =
    List.sort (fun (_, count1) (_, count2) -> compare count2 count1) lst;;
  



  



