let rec drop arr n =
    let rec aux arr n idx = match arr with
    | [] -> []
    | (x::xs) -> if idx = n then aux xs n 1 else x::aux xs n (idx + 1)
    in aux arr n 1
