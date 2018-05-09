(*
**	Machine - A stack-based machine as a compilation target
*)

open Types

(* machine_exec
   Runs a machine program in an fresh machine *)
val machine_exec : machine_program -> unit

(* machine_compile
   Compiles a Fouine program into an equivalent stack-machine program *)
val machine_compile : program -> machine_program
