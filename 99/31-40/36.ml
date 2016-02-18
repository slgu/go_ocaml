let is_prime = function
    | 1 -> false
    | 2 -> true
    | x -> let rec aux idx =
        if idx * idx > x then true
        else if (x mod idx) = 0 then false
        else aux (idx + 1)
        in aux 2

let rec all_primes l r =
    if l > r then []
    else if is_prime l then l :: all_primes (l + 1) r
    else all_primes (l + 1) r
