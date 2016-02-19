type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec internals = function
    | Empty -> []
    | Node (_, Empty, Empty) -> []
    | Node (x, y, z) -> (x :: internals y) @ internals z
