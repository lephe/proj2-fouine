(* Simple lists with type constructors behind *)

let rec list_length l = match l with
| Empty _ -> 0
| Cons (hd, tl) -> 1 + list_length tl
;;

prInt (list_length [1; 2; 3; 4] + list_length (1 :: 2 :: 3 :: []) - 7)
