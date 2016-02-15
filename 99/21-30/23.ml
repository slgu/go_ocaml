let rec drop arr n =
    let rec aux arr n idx = match arr with
    | [] -> []
    | (x::xs) -> if idx = n then aux xs n (idx + 1) else x::aux xs n (idx + 1)
    in aux arr n 1

let rec rand_select arr n =
    if n = 0 then [] else
        let l = List.length arr
        in let rand_pos = Random.int l
        in (List.nth arr rand_pos)::rand_select (drop arr (rand_pos + 1)) (n - 1)



(*
    function with pattern match
*)
