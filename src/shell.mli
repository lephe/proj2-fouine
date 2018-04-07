(*
**	Shell - Interactive Read-Eval-Print Loop shell
*)

open Types

(* shell_main [config -> unit]
   Starts an interactive REPL session. Returns when the user leaves the
   interpreter *)
val shell_main : config -> unit
