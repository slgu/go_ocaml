type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec leaves = function
    | Empty -> []
    | Node (x, Empty, Empty) -> [x]
    | Node (_, y, z) -> leaves y @ leaves z
