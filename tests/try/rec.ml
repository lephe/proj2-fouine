let rec f x =
	try if x = 100 then raise 10 else f (x + 1) with
	| y -> if x = y then y else raise y

in prInt (f 0 - 10)
