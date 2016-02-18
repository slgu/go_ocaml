let is_prime = function
    | 1 -> false
    | 2 -> true
    | x -> let rec aux idx =
        if idx * idx > x then true
        else if (x mod idx) = 0 then false
        else aux (idx + 1)
        in aux 2

let goldbach n =
    let rec aux n idx =
        if is_prime idx && is_prime (n - idx) then (idx, n - idx)
        else aux n (idx + 1)
    in aux n 2
