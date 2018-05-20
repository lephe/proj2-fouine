(*
**	Machine - A stack-based machine as a compilation target
*)

open Types

(* machine_exec
   Runs a machine program in an fresh machine. Applied transformations are
   indicated to initialize the environment with appropriate values. *)
val machine_exec : machine_program -> char list -> unit

(* machine_compile
   Compiles a Fouine program into an equivalent stack-machine program *)
val machine_compile : program -> char list -> machine_program
