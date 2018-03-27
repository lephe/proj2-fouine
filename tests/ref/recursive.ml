(* Recursively-generated reference *)
let rec f x =
	if x <= 1 then ref x else
	let r = f (x - 1) in r := x * !r; r
;;

prInt !(f 10)
