type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let rec decode arr =
    let rec repeated x y = if x == 0 then [] else y::(repeated (x - 1) y)
    in match arr with
    | ((Many (a,b))::xs) -> List.concat [repeated a b;decode xs]
    | ((One a)::xs) -> a::decode xs
    | [] -> []
