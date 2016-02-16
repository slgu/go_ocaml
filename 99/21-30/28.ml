let rec insert item cmp = function
    | [] -> [item]
    | (x::xs) as l -> if cmp item x then item::l else x::insert item cmp xs

let rec sort cmp = function
    | [] -> []
    | (x::xs) -> insert x cmp (sort cmp xs)

let length_sort arr =
    let map_length_arr = List.map (fun item -> List.length item, item) arr
    in let sort_cmp (x1, y1) (x2, y2) = x1 < x2
    in List.map (fun (item1, item2) -> item2) (sort sort_cmp map_length_arr)

let frequency_sort arr =
    let module MapInt = Map.Make(Int32)
    and let mp = List.fold_left (fun mp item ->
            let l = Int32.of_int (List.length item) in
            try MapInt.add l ((MapInt.find l mp) + 1) mp with
                Not_found -> MapInt.add l 1 mp
        ) MapInt.empty arr
    in let sort_cmp x1 x2 =
        let l1 = Int32.of_int (List.length x1)
        and l2 = Int32.of_int (List.length x2)
        in (MapInt.find l1 mp) < (MapInt.find l2 mp)
    in sort sort_cmp arr
