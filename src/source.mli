(*
**	Source - Re-interpretable representations of objects
*)

open Types

(* source_expr
   Produces a re-interpretable listing of an expression *)
val source_expr : expr -> int -> string

(* source_statement
   Returns a re-interpretable representation of a statement *)
val source_statement : statement -> string

(* source_program
   Builds a program listing out of the parsed representation *)
val source_program : program -> string

(* source_machine
   Returns an assembler listing of a machine program *)
val source_machine : machine_program -> string
