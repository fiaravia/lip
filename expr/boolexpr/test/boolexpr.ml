open BoolexprLib.Main
open BoolexprLib.Ast

let test_eval expr exp_result =
  (expr |> parse |> eval) = exp_result

(* ### Unit tests for task 4 *)

let%test "test_eval_false" = test_eval "false" false
let%test "test_eval_true" = test_eval "true" true
let%test "test_eval_if_true_false_true" = test_eval "if true then false else true" false
let%test "test_eval_if_false_false_true" = test_eval "if false then false else true" true
let%test "test_eval_nested_if_1" = test_eval "if true then (if true then false else true) else (if true then true else false)" false
let%test "test_eval_nested_if_2" = test_eval "if (if false then false else false) then (if false then true else false) else (if true then false else true)" false
let%test "test_eval_nested_if_3" = test_eval "if (if (if false then false else false) then (if false then true else false) else (if true then false else true)) then (if false then true else false) else (if true then false else true)" false

(* ### Unit tests for task 5 *)



let%test "test_trace1_1" = try(let e = (parse "if true then false else true" |> trace1) in e = False) with NoRuleApplies -> false
let%test "test_trace1_2" = try(let e = (parse "if (if true then true else false) then false else true" |> trace1) in e = If(True,False,True) ) with NoRuleApplies -> false

let%test "test_trace1_3" = try(let e = (parse "true" |> trace1) in e = True) with NoRuleApplies -> (is_value (parse "true"))

let%test "test_trace1_4" = List.length (parse "if (if false then false else false) then (if false then true else false) else (if true then false else true)" |> trace) <=10

let%test "test &&" = test_eval "if true && true then false else true" false
let%test "test ||" = test_eval "if true || false then false else true" false
let%test "test && ||" = test_eval "if true && (true || false) then false else true" false