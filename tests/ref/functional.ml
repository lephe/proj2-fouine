(* References to functions *)
let square x = x * x
let cube x = x * x * x;;

let x = ref square in
x := cube;
prInt (!x 5)
