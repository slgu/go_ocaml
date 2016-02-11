let rec last_two arr = match arr with
    | [x;y] -> Some x
    | x :: xs -> last_two xs
    | _ -> None
