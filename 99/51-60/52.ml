type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let at_level tree n =
    let rec aux tree n idx = if idx > n then []
        else match tree with
            | Empty -> []
            | Node (x, y, z) -> if n = idx then [x] else aux y n (idx + 1) @ aux z n (idx + 1)
    in aux tree n 1
