let factors n =
    let rec aux n num =
        if n mod num = 0 then match aux (n / num) num with
            | (h, n) :: t when h = num -> (h, n + 1) :: t
            | l -> (num, 1) :: l
        else if n < num * num then [(n, 1)]
        else aux n (num + 1)
    in aux n 2
