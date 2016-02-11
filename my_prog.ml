let () = print_endline (range 1 3);;
let average a b = (a +. b) /. 2.0;;
let rec range a b =
    if a > b then []
    else a  :: range(a + 1) b;;
