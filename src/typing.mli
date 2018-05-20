(*
**	Typing - type inference
*)

open Types

(* type_infer
   Infers the most general type of the provided expression within the given
   environment *)
val type_infer : expr -> env -> ptype

(* type_breakdown
   Breaks down a type to match a pattern, returning the types of all free
   variables of the pattern (in a given environment) *)
val type_breakdown : ptype -> pattern -> env -> (string * ptype) list
