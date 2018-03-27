(* Simple list functions using an ADT *)
type list = Empty | Cons

let rec list_length l = match l with
| Empty _ -> 0
| Cons (hd, tl) -> 1 + list_length tl

let rec list_max l = match l with
| Empty _ -> 0
| Cons (hd, tl) ->
	let max_tl = list_max tl in
	if hd > max_tl then hd else max_tl
;;

let l = Cons (5, Cons (3, Cons (8, Empty ())))
;;

prInt (list_length l + list_max l - 11)
