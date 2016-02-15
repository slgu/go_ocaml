(*
    implement using clojure thinking
*)
let extract k list =
    let rec aux k acc emit = function
        | [] -> acc
        | (x::xs) -> if k = 1 then aux k (emit [x] acc) emit xs else
            let new_emit y = emit (x::y)
            in aux k (aux (k - 1) acc new_emit xs) emit xs
    in let emit x acc = (x::acc)
    in aux k [] emit list
