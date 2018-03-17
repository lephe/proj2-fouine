(* Shadow values by re-binding the same name *)
let x = (let x = 2 in x * x) + 2 in
prInt (x + (let x = 5 in x) * x)
