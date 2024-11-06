open Toyparser.Main

let%test "test_eval_1" = parse "1 + 2 + 3 + (1 + 2)" |> eval = Ok 9
let%test "test_eval_2" = parse "1 + 2 + 3 + (1 + 2) + 1" |> eval = Ok 10
let%test "test_subtract" = parse "1 - 2 - 3" |> eval = Ok (-4)
let%test "test_subtract_2" = parse "10 - 2 " |> eval = Ok (8)
let%test "test_multiply" = parse "1 * 2 * 3" |> eval = Ok 6
let%test "test_multiply_2" = parse "10 * 2 " |> eval = Ok 20
let%test "test_divide" = parse "10 / 2 / 5" |> eval = Ok 1
let%test "test_divide_2" = parse "10 / 2 " |> eval = Ok 5
let%test "test_divide_3" = parse "10 / 0 " |> eval = Error "Division by zero"
let%test "test_neg" = parse "-1" |> eval = Ok (-1)
let%test "test_neg_2" = parse "-1 + 2" |> eval = Ok 1
let%test "test_neg_3" = parse "-1 - 2 - -3" |> eval = Ok 0

let%test "test hex" = parse "0x10 + 0x10  + 0x01" |> eval = Ok 33
let%test "test hex_2" = parse "0x1a" |> eval = Ok 26

let%test "test binary" = parse "0b10" |> eval = Ok 2
let%test "test binary_2" = parse "0b10 + 0b10" |> eval = Ok 4

let%test "test octal" = parse "0o10" |> eval = Ok 8

let%test "test mixed bases" = parse "12 + 0b1" |> eval = Ok 13


(* YOUR TESTS HERE *)