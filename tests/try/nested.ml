(* Nested try statements and rethrown exceptions *)
try (try (raise 1) with
	| 0 -> prInt 1
	| 1 -> raise 5
	| 2 -> prInt 2)
with
| 3 -> prInt 4
| 5 -> prInt 0
| _ -> prInt 7
