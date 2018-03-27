(* Mix let-type arguments and inline functions *)
let f x = fun y -> let g z t = fun u -> x + y + z + t + u in g in
prInt (f 1 2 3 4 5)
