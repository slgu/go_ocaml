#load "unix.cma";;
let timeit f param =
    let st = Unix.gettimeofday() in
    let _ = f param
    in let ed = Unix.gettimeofday()
    in ed -. st
