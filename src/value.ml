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
	| V_Closure (_, None, _, _) -> "<closure>"
	| V_Closure (_, Some _,  _, _) -> "<recursive closure>"
	| V_Tuple l ->
		let f v = value_type v env in
		"(" ^ String.concat " * " (List.map f l) ^ ")"
	(* TODO: value_type: Manually specify the type of built-in functions *)
	| V_Builtin f	-> "<builtin>"
	| V_Memory (_,m)-> "<builtin memory>"
	(* Machine-related types *)
	| V_MachineClosure	(_, _, _)	-> "<machine closure>"
	| V_MachineFrame	(_, _)		-> "<machine frame>"
	| V_MachineBuiltin	(_)			-> "<machine builtin>"
