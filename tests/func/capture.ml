let curry f x =
	let y = f x in (fun z -> y z);;

let y = (fun x -> x + 1000);;

let add2 = curry (fun a b -> a + b) 2 in
prInt (add2 8 + y (-1000))
