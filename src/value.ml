(*
**	Value - Concrete data types
*)

open Types

(* value_type [value -> env -> string]
   Returns a value's type, withing a given typing environment *)
let rec value_type v env = match v with
	| V_Int _		-> "int"
	| V_Bool _		-> "bool"
	| V_Unit		-> "unit"
	| V_Ref _		-> "ref"
	| V_Ctor (c, _)	-> StringMap.find c env.types
	| V_Rec			-> "<recursion placeholder>"
	| V_Closure (_, None, _, _) -> "<closure>"
	| V_Closure (_, Some _,  _, _) -> "<recursive closure>"
	| V_Tuple l ->
		let f v = value_type v env in
		"(" ^ String.concat " * " (List.map f l) ^ ")"