(* A more (useless) elaborate example of function with several sub-functions
   Calculates 1 + 2 * x + ... + 5 * x^4 *)
let step4 x p = 5 * p
let step3 x p = 4 * p + step4 x (x * p)
let step2 x p = 3 * p + step3 x (x * p)
let step1 x p = 2 * p + step2 x (x * p)
let func x = 1 + step1 x x
;;
prInt (func 10)
;;
