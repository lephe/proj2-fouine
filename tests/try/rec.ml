(* Nest a lot of try..with and see what happens *)

let rec f x =
	try if x = 0 then raise (E 10) else f (x - 1) with
	| E y -> if y = 0 then y else raise (E (y - 1))

in prInt (f 15)
