(* Order of bindings matter when names are overridden *)
let f (x, y) (z, y, t) (x, k) = x + y + z + t + k in
prInt (f (1, 2) (3, 4, 5) (6, 7))
