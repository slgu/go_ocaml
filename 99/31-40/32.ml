let factors n =
    let rec aux n num =
        if n mod num = 0 then num::aux (n / num) num
        else if n < num * num then [n]
        else aux n (num + 1)
    in aux n 2
