(* Reference escapes scope *)
let make_ref x = ref x;;

prInt !(make_ref 4)
