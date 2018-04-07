(*
**	Errors - Error messages and logs in terminal
*)

(* errors_try
   Executes the provided function, catching possible errors and reporting them
   on stderr. Returns true if an exception occurred, false otherwise *)
val errors_try : (unit -> 'a) -> bool
