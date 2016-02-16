let is_prime = function
    | 1 -> false
    | 2 -> true
    | x -> let rec aux idx =
        if idx * idx > x then true
        else if (x mod idx) = 0 then false
        else aux (idx + 1)
        in aux 2
