(* Undefined names, for many reasons *)
let f =
	if x <= 1 then x else
	f (x - 1) + f (x - 2)
