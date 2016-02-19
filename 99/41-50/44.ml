type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec is_symmetric tree =
    let rec is_mirror a b = match a, b with
        | Empty, Empty -> true
        | Node (_, y1, z1), Node(_, y2, z2) -> is_mirror y1 z2 && is_mirror z2 y2
        | _ -> false
    in match tree with
    | Empty -> true
    | Node (_, y, z) -> is_mirror y z
