type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec count_leaves = function
    | Empty -> 0
    | Node (_, Empty, Empty) -> 1
    | Node (_, y, z) -> count_leaves y + count_leaves z
