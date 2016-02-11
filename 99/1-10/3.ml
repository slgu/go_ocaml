let rec at n arr = match arr with
    | [] -> None
    | x :: xs -> if n = 1 then Some x else at (n - 1) xs
