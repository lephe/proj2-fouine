(* Uncaught exceptions must be passed up *)
try
	begin try (raise 1) with
	| 0 -> prInt 1
	end
with
| 1 -> prInt 0
