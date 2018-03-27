(* Special matching for lists is even funnier *)

let rec list_max l = match l with
| [] -> -1
| [x] -> x
| hd :: tl -> let m = list_max tl in if hd > m then hd else m
in

let rec list_sum4 l = match l with
| [a; b; c; d] -> a + b + c + d
in

prInt (list_max [1; 4; 2] + list_sum4 [1; -2; -3; 0])
