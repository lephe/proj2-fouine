(* Toplevel nested lets with references to previous definitions *)
let x = 4
let y = x + 2
let z = x * y - 4;;
prInt (x + y + z)
