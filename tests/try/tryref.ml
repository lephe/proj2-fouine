(* Make sure that data edited through references is preserved when exceptions
   are raised *)

let r = ref 0 in
let a =
  try (r := !r + 1; raise (E 0))
  with | E y -> 12 in
prInt 0
