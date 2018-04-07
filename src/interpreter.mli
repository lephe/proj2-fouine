(*
**	Interpreter - The higher-level interface for program execution
**	This is where statements are evaluated and where language-related
**	decisions, type inference and interaction with the shell happen.
*)

open Types

(* interpreter_start
   Creates a new execution environment for a program *)
val interpreter_start : unit -> env

(* interpreter_exec
   Executes a statement and returns the updated environment along with a list
   of events, for post-processing (such as showing in terminal) *)
val interpreter_exec : statement -> env -> env * event list
