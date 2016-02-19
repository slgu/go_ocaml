(* contruct h-bal-tree by number of nodes *)

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

let rec range l r = if l = r then [r] else l :: range (l + 1) r

let hbal_tree_nodes n =
    (* with height h minimum node*)
    let rec min_nodes = function
        | 0 -> 0
        | 1 -> 1
        | x -> let x1 = min_nodes (x - 1) and x2 = min_nodes (x - 2) in x1 + x2 + 1
    in
    (*with node n maximum height *)
    let rec max_height = function
        | 0 -> 0
        | 1 -> 1
        | x -> let h = max_height (x - 1) in if min_nodes (h + 1) > x then h else h + 1
    (*calculate how many nodes in a tree*)
    in let rec cal = function
        | Empty -> 0
        | Node (x, y, z) -> 1 + cal y + cal z
    in let h2 = max_height n and h1 = 1
    in let possible_res = List.flatten (List.map (fun h_item -> hbal_tree h_item) (range h1 h2))
    in List.filter (fun tree -> cal tree = n) possible_res
