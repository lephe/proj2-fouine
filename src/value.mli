(*
**	Value - Concrete data types
*)

open Types

(* value_type
   Returns a value's type, withing a given typing environment *)
(* TODO: Make value_type return a ptype object instead of a string *)
val value_type : value -> env -> string
