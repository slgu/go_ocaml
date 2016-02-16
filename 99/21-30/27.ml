let extract k list =
    let rec aux k acc1 acc2 emit1 emit2 = function
        | [] -> (acc1, acc2)
        | (x::xs) -> if k = 1 then
            let new_emit2 y = emit2 (x::y)
            in aux k (emit1 [x] acc1) (emit2 xs acc2) emit1 new_emit2 xs else
            let new_emit1 y = emit1 (x::y)
            in let new_emit2 y = emit2 (x::y)
            in let new_acc1, new_acc2 = aux (k - 1) acc1 acc2 new_emit1 emit2 xs
            in aux k new_acc1 new_acc2 emit1 new_emit2 xs
    in let emit x acc = (x::acc)
    in aux k [] [] emit emit list

let rec group arr = function
    | [x] -> let selected, unselected = extract x arr
        in List.map (fun item -> [item]) selected
    | (x::xs) -> let selected, unselected = extract x arr
    in List.flatten (List.map (fun (s, extra) ->
        let extra_g = group extra xs in
        List.map (fun item -> s::item) extra_g
    ) (List.combine selected unselected))
    | [] -> []


(*
    usage of clojure binding
*)
