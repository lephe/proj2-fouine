(* Nested references *)
let r = ref 12
let a = ref r
let b = a
let s = ref 8
;;

(ref s) := ref 7;
!(ref b) := s;
prInt (!(!a) + !r)
