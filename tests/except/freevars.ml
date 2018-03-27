(* Free variables must be defined by the time the closure is built *)
let f x = x + y
let y = 12;;
prInt (f 4)
