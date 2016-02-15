let rec duplicate arr = match arr with
    | x::xs -> x::x::(duplicate xs)
    | [] -> []
