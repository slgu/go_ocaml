let compress arr =
    let rec aux res arr = match arr with
        | [] -> []
        | [x] -> x::res
        | x::y::xs -> if x = y then aux res (y::xs) else aux (x::res) (y::xs)
    in List.rev (aux [] arr)
