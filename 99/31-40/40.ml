type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;
let get_some_val = function
    | Some x -> x
    | None -> false

let rec eval var_arr = function
    | Var str ->
        get_some_val (List.fold_left (fun item (var, value) ->
            if item = None && var = str then Some value else item) None var_arr)
        (*List.assoc is ok, just search a key in a list of (key,value) to get the value *)
    | Not nest_expr -> not (eval var_arr nest_expr)
    | And (nest_expr1, nest_expr2) -> begin match eval var_arr nest_expr1, eval var_arr nest_expr2 with
            | true, true -> true
            | _, _ -> false
        end
    | Or (nest_expr1, nest_expr2) -> begin match eval var_arr nest_expr1, eval var_arr nest_expr2 with
            | false, false -> false
            | _, _ -> true
        end

let table var_arr expr =
    let rec generate_bool = function
        | 1 -> [[false];[true]]
        | n -> let res = generate_bool (n - 1)
            in List.fold_left (fun arr item -> (false::item)::(true::item)::arr) [] res
    in let l = List.length var_arr
    in let bool_values = generate_bool l
    in List.map (fun item -> let var_arr = List.combine var_arr item in (var_arr, eval var_arr expr) ) bool_values
