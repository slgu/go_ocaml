let rec slice arr st ed =
    let rec aux arr idx = match arr with
        | (x::xs) -> if idx >= st && idx <= ed then x::aux xs (idx + 1)
            else aux xs (idx + 1)
        | [] -> []
    in aux arr 0
