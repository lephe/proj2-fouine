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

(* expr_eval_k
   A continuation-style version of expr_eval with an additional set of strings
   indicating which names are begin recursively defined *)
val expr_eval_k : expr -> env -> StringSet.t -> (value -> value) -> value
