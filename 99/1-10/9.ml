let rec pack arr =
    let rec aux now res arr = match arr with
        | [] -> []
        | (x::(y::xs as t)) -> if x = y then aux (x::now) res t else aux [] ((x::now)::res) t
        | [x] -> ((x::now)::res)
    in List.rev (aux [] [] arr)
