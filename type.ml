open Printf
let arr = [1;2;3];;
type pair_of_ints = { a : int; b : int };;
let test_pair_of_ints = {a=3; b=5};;
type foo =
    | Nothing
    | Int of int
    | Pair of int * int
    | String of string;;
let tuple = (3, "231");;
type expr =
    | Plus of expr * expr
    | Times of expr * expr
    | Minus of expr * expr
    | Divide of expr * expr
    | Value of int

let rec to_string s =
    match s with
    | Plus (left, right) ->
        "(" ^ to_string left ^ " + " ^ (to_string right) ^ ")"
    | Times (left, right) ->
        "(" ^ to_string left ^  " * " ^ to_string right ^ ")"
    | Minus (left, right) ->
        "(" ^ to_string left ^  " - " ^ to_string right ^ ")"
    | Divide (left, right) ->
        "(" ^ to_string left ^  " / " ^ to_string right ^ ")"
    | Value a ->
        string_of_int a

let print_expr e =
    print_endline (to_string e)

let () =
    print_expr (Times (Value 1, Value 2));
    eprintf "fuck"
