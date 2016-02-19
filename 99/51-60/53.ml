type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

(*ocaml guard when y = xxx *)
let rec simple_log x idx exp_idx =
    if exp_idx > x then idx
    else simple_log x (idx + 1) (exp_idx * 2)

let rec pow x y = if y = 0 then 1 else x * pow x (y - 1)

let judge_lr n =
    let h = simple_log (n + 1) 1 2
    in n <= ((pow 2 h) - 2 - (pow 2 (h - 2)))

let rec complete_binary_tree list = match list with
    | [] -> Empty
    | [x] -> Node(x, Empty, Empty)
    | _ -> let arr_with_idx = List.rev (List.fold_left (
            fun arr item -> begin match arr with
                    | [] -> [(0, item)]
                    | ((last_idx, _)::extra as t) -> (last_idx + 1, item) :: t
                end
            ) [] list)
            in let (idx, v)::extra = arr_with_idx
            in let left_part, right_part = List.partition (
                    fun (idx, item) -> judge_lr idx
                ) extra
            in let left_part = List.map (fun (idx,item) -> item) left_part
                and right_part = List.map (fun (idx,item) -> item) right_part
            in Node (v, complete_binary_tree left_part, complete_binary_tree right_part)
