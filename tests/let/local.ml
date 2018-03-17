(* Using let bindings locally, inside expressions *)
let x = 4 in
let z = x + (let y = 8 in y * (y + 1)) in
prInt (x * z)
