(* Return a function that recursively creates a new function... *)

let get_power ignored =
	let rec power n =
		if n = 1 then (fun x -> x) else
		fun x -> x * power (n - 1) x
	in power
;;

let pow = get_power ();;
prInt (get_power () 4 5);;
prInt (pow 3 8);;
