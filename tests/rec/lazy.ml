let rec f = fun x ->
	if x = 0 then 1 else f 0
in prInt (f 5)
