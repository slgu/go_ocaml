let rec replicate arr n =
    let rec repeated a b = if a == 0 then [] else b::(repeated (a - 1) b)
    in match arr with
    | (x::xs) -> List.concat [(repeated n x);replicate xs n]
    | [] -> []
