let rec rev arr = match arr with
    | [] -> []
    | [x] -> [x]
    | x :: xs -> List.append (rev xs) [x]
