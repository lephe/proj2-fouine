(*
**	Interpreter - The higher-level interface for program execution
**	This is where statements are evaluated and where language-related
**	decisions, type inference and interaction with the shell happen.
*)

open Types

(* interpreter_start
   Creates a new execution environment for a program. The base environment
   depends on the transformations applied on the source code, which may only be
   ['R'], ['E'], ['R';'E'] or ['E';'R'] *)
val interpreter_start : char list -> env

(* interpreter_exec
   Executes a statement and returns the updated environment along with a list
   of events, for post-processing (such as showing in terminal). The boolean
   argument indicates whether type inference should be done *)
val interpreter_exec : bool -> statement -> env -> env * event list
