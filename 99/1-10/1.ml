let rec last arr = match arr with
    | [] -> None
    | [x] -> Some x
    | x :: xs -> last xs
