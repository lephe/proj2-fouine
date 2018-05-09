(* A more sophisticated example of interrupted flow *)

let f x =
	raise (E (let _ = raise (E 12) in x))

let _ =
	prInt(try f 100 with E y -> y - 12)
