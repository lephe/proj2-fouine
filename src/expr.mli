(*
**	Expr - Expression representation and evaluation
*)

open Types

(* expr_free
   Returns the set of free variables in an expression *)
val expr_free : expr -> StringSet.t

(* expr_eval
   Recursively evaluates a single expression within an environment, yielding a
   value (or throwing an exception). The environment argument contains both
   values and types, which makes this function pure *)
val expr_eval : expr -> env -> value
