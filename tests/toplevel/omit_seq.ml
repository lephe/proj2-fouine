(* More complex example of toplevel sequence with omitted semicolon pairs *)
let x = 2
let y = 3;;
let z = 4;;
let t = 5
let u = 6
let v = 1;;
prInt (x * y * z * t * u * v - 720)
