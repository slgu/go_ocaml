(*
    string index get str.[i]
*)
let my_list = [1;2;3;4;5;6;7];;
let () =
    let f elem = Printf.printf "I'm looking at element %d now\n" elem in
    let new_list = List.map (( * ) 2) my_list
    in List.iter f new_list;;

(*
ML衍生的语言，如OCaml是“几乎纯”的。它们允许引用和数组引入一定的副作用，但很大程度上 你会写纯函数，
而语言本身也鼓励你这么做。另一个函数式语言Haskell是纯的（如果不考虑IO模块）。 相比，OCaml更加实用，因为非纯的函数有时候还是很有用的。
*)

(*
The way to think of a boxed object is to think of an object which has been allocated on the heap using malloc in C
(or equivalently new in C++),
*)
