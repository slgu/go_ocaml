let pre_insert c s =
    (String.make 1 c) ^ s
let rec gray n =
    if n <= 1 then ["0";"1"]
    else let extra = gray (n - 1)
    in (List.map (fun item -> pre_insert '0' item) extra) @
        (List.map (fun item -> pre_insert '1' item) (List.rev extra))
