type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let combine left_trees right_trees =
    List.flatten (List.map (fun right_tree -> List.map (fun left_tree -> Node ('x', left_tree, right_tree)) left_trees) right_trees)

(* contruct h-bal-tree by height *)
let rec hbal_tree = function
    | 0 -> [Empty]
    | 1 -> [Node ('x', Empty, Empty)]
    | x ->
        let extra_trees1 = hbal_tree (x - 1) and extra_trees2 = hbal_tree (x - 2)
        in combine extra_trees1 extra_trees2 @ combine extra_trees2 extra_trees1 @ combine extra_trees1 extra_trees1
