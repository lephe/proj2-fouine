(* Just like the open recursion example, a fixed-point combinator *)

let fact_open f x =
	if x <= 1 then x else
	x * f (x - 1)

let pow_open f x n =
	if n = 1 then x else
	x * f x (n - 1)

(* As expected the right-hand side must be a function, otherwise the evaluation
   loops forever, so the combinator will only work on functions *)
let rec fix f = fun arg -> f (fix f) arg

let fact = fix fact_open
let pow  = fix pow_open
;;

prInt (fact 4 + pow 7 3)
