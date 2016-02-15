let rec remove_at n arr =
    let rec aux arr idx = match arr with
        | (x::xs) -> if idx == n then aux xs (idx + 1) else x::aux xs (idx + 1)
        | [] -> []
    in aux arr 0
