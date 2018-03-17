(* Normally overflow should happen as in OCaml *)
let _ = prInt (100000 * 100000 * 100000 * 100000);;
let a = 4611686018427387903
let _ = prInt (a + 1);;
let _ = prInt (-a - 1);;
