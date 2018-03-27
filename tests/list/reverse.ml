(* Reverse a list by using it as a stack *)

let rec list_rev src dst = match src with
| Empty _ -> dst
| Cons (hd, tl) -> list_rev tl (Cons (hd, dst))
;;

let sum_three_first Cons (x0, Cons (x1, Cons (x2, tl))) = x0 + x1 + x2
;;

prInt (sum_three_first (list_rev [1; 2; 3; -7; 4] []))
