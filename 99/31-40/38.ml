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

let rec goldbach_list a b =
    if a > b then []
    else if a mod 2 = 0 then (a, goldbach a) :: goldbach_list (a + 1) b
    else goldbach_list (a + 1) b

let rec goldbach_limit a b limit =
    if a > b then []
    else if a mod 2 = 0 then
        let x, y = goldbach a in
            if x > limit then (a, (x, y)) :: goldbach_limit (a + 1) b limit
            else goldbach_limit (a + 1) b limit
    else goldbach_limit (a + 1) b limit
