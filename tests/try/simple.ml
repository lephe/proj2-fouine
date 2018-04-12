(* The most basic kind of try, with pattern matching *)
try (raise 1) with
| 0 -> prInt 5
| 1 -> prInt 0
| _ -> prInt 12
