let split arr =
    let sorted_arr = List.sort (fun (a1, v1) (a2, v2) -> v1 - v2) arr
    in match sorted_arr with
        | (x1::x2::xs) -> ([x1;x2], xs)
        | _ -> ([],[])

let concat_protocol x y =
    x ^ "_" ^ y

let combine arr = match arr with
    | [(a1,v1);(a2,v2)] -> (concat_protocol a1 a2, v1 + v2)
    | _ -> ("",0)

let rec huffman fs = match fs with
    | [(a,v)] -> [(a, "0")]
    | [(a1,v1);(a2,v2)] -> [(a1, "0");(a2, "1")]
    | otherwise ->
        let [(l1, v1);(l2, v2)] as leafs, others = split fs
        in let new_node_label, new_node_weight = combine leafs
        in let extra_res = huffman ((new_node_label, new_node_weight)::others)
        in List.fold_left (fun arr (a, enc) ->
            if new_node_label = a then (l1, enc ^ "0")::(l2, enc ^ "1")::arr else (a, enc)::arr)
            [] extra_res
