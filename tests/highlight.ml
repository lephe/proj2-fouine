(* This is a special tests, to check that Fouine properly highlights the
   relevant section of the source file when an error occurs *)

let faulty_addition x =
	x + (fun x ->
		if 2 * (x / 2) = x then x / 2
		else 3 * x + 1)
;;

prInt (faulty_addition 8)
