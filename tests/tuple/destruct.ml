(* Destruct tuples by binding *)
let bundle = (1, (2, 3));;

let (a, x) = bundle in
	let (b, c) = x in
		prInt (a + b + c);;

let (a, (b, c)) = bundle in
	prInt (a + b + c)
