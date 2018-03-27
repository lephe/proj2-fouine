(* A very imaginative example of a recursive function *)
let rec fact n =
	if n <= 1 then n
	else n * fact (n - 1)
;;

prInt (fact 6)
