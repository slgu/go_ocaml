let rec gcd x y = if y = 0 then x else if x >= y then gcd y (x mod y) else gcd y x
let coprime x y = (gcd x y) = 1
let phi n =
    let rec generate = function
        | 1 -> [1]
        | x -> x::generate (x - 1)
    in List.fold_left (fun x y -> if coprime n y then x + 1 else x) 0 (List.rev (generate n))
