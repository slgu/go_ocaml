type 'a node =
    | One of 'a
    | Many of 'a node list;;


let rec flatten nest_list =
    let rec aux nest_list = match nest_list with
        | One x -> [x]
        | Many arr -> match arr with
            | [] -> []
            | _ -> List.fold_left (fun arr x -> List.append arr (aux x)) [] arr
    in List.fold_left (fun arr x -> List.append arr (aux x)) [] nest_list
