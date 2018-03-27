(* Aliasing two references - editing one changes the other *)
let x = ref 4 in
let y = x in
y := 8; prInt !x
