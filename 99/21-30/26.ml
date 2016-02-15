let rec extract arr n = match (List.length arr) < n, n = 0 with
    | true, _ -> []
    | _, true -> []
    | otherwise -> begin
        match arr with
        | [] -> []
        | (x::xs) -> (if n = 1 then [[x]] else (List.map (fun item -> x::item) (extract xs (n - 1))))
            @ extract xs n
        end
