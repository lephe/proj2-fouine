(* Basic operations on references *)
let x = ref 2 in
x := 4;
x := !x * !x + 2;
prInt !x
