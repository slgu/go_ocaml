let rec split arr n =
    let rec aux arr fpart idx = match arr with
        | (x::xs) -> if idx = n then (List.rev (x::fpart), xs)
            else aux xs (x::fpart) (idx + 1)
        | [] -> ([],[])
    in aux arr [] 1

let rec rotate arr n =
    let fpart, spart = split arr n
    in spart @ fpart


(*
    @ to concat two list
*)
