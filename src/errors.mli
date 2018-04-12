(*
**	Errors - Error messages and logs in terminal
*)

(* errors_try
   Executes the provided function, catching possible errors and reporting them
   on stderr. Returns None if an exception occurred, the result of the function
   call otherwise *)
val errors_try : (unit -> 'a) -> 'a option
