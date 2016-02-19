type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let construct arr =
    let rec insert tree item = match tree with
        | Empty -> Node(item, Empty, Empty)
        | Node (x, y, z) -> if x >= item then Node (x, insert y item, z)
            else Node (x, y, insert z item)
    in List.fold_left (fun tree item -> insert tree item) Empty arr
