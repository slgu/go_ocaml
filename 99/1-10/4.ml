let rec length arr = match arr with
    | [] -> 0
    | x :: xs -> 1 + (length xs)
