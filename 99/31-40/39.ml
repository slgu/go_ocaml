type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

let rec table2 a b expr =
    let rec eval a b va vb = function
        | Var str -> if str = a then va else vb
        | Not nest_expr -> not (eval a b va vb nest_expr)
        | And (nest_expr1, nest_expr2) -> begin match eval a b va vb nest_expr1, eval a b va vb nest_expr2 with
                | true, true -> true
                | _, _ -> false
            end
        | Or (nest_expr1, nest_expr2) -> begin match eval a b va vb nest_expr1, eval a b va vb nest_expr2 with
                | false, false -> false
                | _, _ -> true
            end
    in List.map (fun (ia, ib) -> (ia, ib, eval a b ia ib expr))
        [(false, false);(false, true); (true,false);(true,true)]
