type 'a rle =
    | One of 'a
    | Many of int * 'a;;
let encode arr =
    let rec aux arr cnt = match arr with
        | (x::((y::xs) as t)) -> if x = y then aux t (cnt + 1) else (Many (cnt + 1, x))::aux t 0
        | [x] -> [Many (cnt + 1, x)]
        | [] -> []
    in  aux arr 0
