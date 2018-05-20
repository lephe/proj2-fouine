(* Simple list functions using an ADT *)
type mylist = MyEmpty of unit | MyCons of int * mylist

let rec list_length l = match l with
| MyEmpty _ -> 0
| MyCons (hd, tl) -> 1 + list_length tl

let rec list_max l = match l with
| MyEmpty _ -> 0
| MyCons (hd, tl) ->
	let max_tl = list_max tl in
	if hd > max_tl then hd else max_tl
;;

let l = MyCons (5, MyCons (3, MyCons (8, MyEmpty ())))
;;

prInt (list_length l + list_max l - 11)
