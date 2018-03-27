(* Parsing ambiguity with unary minus has to be resolved in favor of the
   arithmetic operation, not the function call *)
let n = 2
let f x = x + 1
let g x = x * 2;;

let _ = prInt 10;;
let a = g 2 in
let b = n -2 in
	prInt (10 * a + b + f 4)
