(* Order of evaluation for the arguments *)
let f x y = x + y in

prInt (f (prInt 4) (prInt 8) + 2)
