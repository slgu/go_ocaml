let rec split arr n =
    let rec aux arr fpart idx = match arr with
        | (x::xs) -> if idx = n then (List.rev (x::fpart), xs)
            else aux xs (x::fpart) (idx + 1)
        | [] -> ([],[])
    in aux arr [] 1

let rec insert_at item n arr =
    let fpart, spart = split arr n in fpart @ [item] @ spart
