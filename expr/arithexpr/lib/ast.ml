type expr =
    True
  | False
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | If of expr * expr * expr
  | Zero
  | Succ of expr
  | Pred of expr
  | IsZero of expr

let is_value = function
  | True | False | Zero -> true
  | _ -> false

  type exprval = Bool of bool | Nat of int