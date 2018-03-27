(* Non-exhaustive pattern matching *)
type tx = Tx | Ty;;

match Tx () with
| Ty _ -> prInt 4
