(* Non-exhaustive pattern matching *)
type tx = Tx of int | Ty of int;;

match Tx 2 with
| Ty _ -> prInt 4
