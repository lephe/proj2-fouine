(*
**	Pattern - Pattern-matching expressions for let, match, functions...
*)

open Types

(* pattern_free [pattern -> StringSet.t]
   Returns a list of all names bound by the pattern *)
let rec pattern_free pat = match pat with
	| Wildcard -> StringSet.empty
	| Identifier i -> StringSet.singleton i

(* pattern_str [pattern -> string]
   Returns a textual representation of a pattern *)
let pattern_str p = match p with
	| Wildcard -> "_"
	| Identifier i -> i

(* pattern_unify [pattern -> value -> (string, value) list]
   Implements syntactic unification of first-order terms to bind a pattern to
   a value, or raise a pattern-matching exception
   TODO: Reject matching if a variable is bound several times *)
let pattern_unify pat (v: value) = match pat with
	| Wildcard -> []
	| Identifier i -> [ (i, v) ]
