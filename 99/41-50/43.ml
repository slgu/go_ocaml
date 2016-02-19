(*begin binary tree problems *)
type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec cbal_tree = function
    | 0 -> [Empty]
    | 1 -> [Node('x', Empty, Empty)]
    | x ->
        if ((x - 1) mod 2) = 0 then
            let extra_trees = cbal_tree ((x - 1) / 2)
            in List.flatten (List.map (fun tree_item -> List.map (fun tree_item2 ->
                Node ('x', tree_item, tree_item2)) extra_trees) extra_trees)
        else
            let extra_trees1 = cbal_tree (x / 2)
                and extra_trees2 = cbal_tree (x / 2 - 1)
            in List.flatten (List.map (fun tree_item -> List.map (fun tree_item2 ->
                Node ('x', tree_item, tree_item2)) extra_trees1) extra_trees2)
                @ List.flatten (List.map (fun tree_item -> List.map (fun tree_item2 ->
                    Node ('x', tree_item, tree_item2)) extra_trees2) extra_trees1)
