let factors n =
    let rec aux n num =
        if n mod num = 0 then match aux (n / num) num with
            | (h, n) :: t when h = num -> (h, n + 1) :: t
            | l -> (num, 1) :: l
        else if n < num * num then [(n, 1)]
        else aux n (num + 1)
    in aux n 2

let phi_improved n =
    let p_m_s = factors n
    in let rec pow n = function
        | 0 -> 1
        | 1 -> n
        | x -> n * pow n (x - 1)
    in List.fold_left (fun x (p, m) -> (pow p (m - 1)) * (p - 1) *  x) 1 p_m_s
