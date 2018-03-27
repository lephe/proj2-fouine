(* OCaml allows omitting the parentheses in most cases *)
let a, b, (x, y) = 1, 2, if true then 7, 8 else 2, 4 in
prInt (a * b - x * y)
