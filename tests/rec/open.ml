(* Fibonacci sequence in open recursion style. In this example the name of the
   recursive function changes from "fibonacci" to "cont", which failed in my
   original immplementation *)

let f cont x =
	if x <= 1 then x else
	cont (x - 1) + cont (x - 2)
;;

let rec fibonacci x = f fibonacci x
;;

prInt (fibonacci 5)
