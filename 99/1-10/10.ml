let rec encode arr =
    let rec aux num arr = match arr with
        | (x::(y::xs as t)) -> if x = y then aux (num + 1) t else (num + 1, x) :: aux 0 t
        | [x] -> [(num  +1, x)]
        | [] -> []
    in aux 0 arr
