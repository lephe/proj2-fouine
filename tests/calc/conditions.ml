(* Condition routing - here without comparison operators *)
if true then
	if false then
		if true then prInt (2 + 3)
		else prInt (5 * 7)
	else
		if false then
			prInt 44
		else
			if true then prInt (6 * 11 * 10 + 6)
			else prInt 57
else
	if true then prInt 65
	else prInt 1
