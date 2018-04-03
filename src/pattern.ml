(*
**	Pattern - Pattern-matching expressions for let, match, functions...
*)

open Types
open Util
open Exceptions

(* pattern_free [pattern -> StringSet.t]
   Returns a list of all names bound by the pattern *)
let rec pattern_free pat = match pat with
	| P_Name i		-> StringSet.singleton i
	| P_Ctor (_, p)	-> pattern_free p
	| P_Tuple l ->
		let map = List.map pattern_free l in
		List.fold_left StringSet.union StringSet.empty map
	| _ -> StringSet.empty

(* pattern_str [pattern -> string]
   Returns a textual representation of a pattern *)
let rec pattern_str p = match p with
	| P_Name i		-> i
	| P_Wildcard	-> "_"
	| P_Int i		-> string_of_int i
	| P_Bool b		-> if b then "true" else "false"
	| P_Unit		-> "()"
	| P_Tuple l		-> "(" ^ String.concat ", " (List.map pattern_str l) ^ ")"
	| P_Ctor (c, p)	-> c ^ " " ^ pattern_str p

(* pattern_dup [private]
   [pattern -> StringSet.t -> StringSet.t -> StringSet.t * StringSet.t]
   Returns the set of all names that are bound multiple times in the pattern.
   @arg  Pattern to check
   @arg  Names that already appeared
   @arg  Duplicates already found
   @ret  The two last arguments, updated *)
let rec pattern_dup pat found dups = match pat with
	| P_Name i ->
		if StringSet.mem i found
		then (found, StringSet.add i dups)
		else (StringSet.add i found, dups)
	| P_Tuple l ->
		List.fold_left (fun (x, y) p -> pattern_dup p x y) (found, dups) l
	| P_Ctor (c, p) ->
		pattern_dup p found dups
	| _ -> (found, dups)

(* pattern_unify [pattern -> value -> (string, value) list]
   Implements syntactic unification (actually filtering) to bind a pattern to
   a value, or raise a pattern-matching exception. Returns a list of bindings
   on the form (name, value). *)
let rec pattern_unify pat (v: value) =
	let fail () = raise (MatchError (range_empty, Some pat, v)) in

	(* Before binding, check that there isn't any duplicate names *)
	let (_, dups) = pattern_dup pat StringSet.empty StringSet.empty in
	if dups <> StringSet.empty
	then raise (MultiBind (false, range_empty, dups)) else

	match (pat, v) with

	(* Wildcards and identifiers always match *)
	| P_Wildcard, _ -> []
	| P_Name i, _ -> [ (i, v) ]

	(* Literal values must be equal *)
	| P_Int i,	V_Int j  when i = j -> []
	| P_Bool b,	V_Bool c when ((b && c) || (not b && not c)) -> []
	| P_Unit,	V_Unit -> []

	(* Tuples is where is gets interesting - recursion kicks in! *)
	| P_Tuple pl, V_Tuple vl ->
		if List.length pl <> List.length vl then fail() else
		let l = List.combine pl vl in
		List.concat (List.map (fun (x, y) -> pattern_unify x y) l)

	(* Constructors are also a simple form of recursion for pattern matching *)
	| P_Ctor (ctor, pat), V_Ctor (candidate, v) ->
		if ctor <> candidate then fail() else
		pattern_unify pat v

	(* All other combinations are rejected as match errors *)
	| _ -> fail()
