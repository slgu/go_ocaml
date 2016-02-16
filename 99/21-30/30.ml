let rec gcd x y = if y = 0 then x else if x > y then gcd y (x mod y) else gcd y x
let coprime x y = (gcd x y) = 1
