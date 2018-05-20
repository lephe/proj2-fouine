
(*
**	Pattern - Pattern-matching expressions for let, match, and functions
*)

open Types
open Range
open Exceptions

(* pattern_free [pattern -> StringSet.t]
   Returns a list of all the names bound by the pattern. This function does not
   indicate whether there are duplicates or not. Only pattern_unify will fail
   in this case *)
let rec pattern_free pat = match pat with
	| P_Name i		-> StringSet.singleton i
	| P_Ctor (_, p)	-> pattern_free p
	| P_Tuple l ->
		let map = List.map pattern_free l in
		List.fold_left StringSet.union StringSet.empty map
	| _ -> StringSet.empty

(* pattern_dup [private]
   [pattern -> StringSet.t -> StringSet.t -> StringSet.t * StringSet.t]
   Returns the set of all names that are bound multiple times in the pattern.
   @pat		Pattern to check
   @found	Names that already appeared
   @dups	Duplicates already found
   Returns	The two last arguments, updated *)
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

(* pattern_unify [pattern -> value -> (string * value) list]
   Implements syntactic unification (actually filtering) to bind a pattern to
   a value, or raise a pattern-matching exception. Returns a list of bindings
   on the form (name, value) on success *)
let rec pattern_unify pat value =
	let fail () = raise (MatchError (range_empty, Some pat, value)) in

	(* Before binding, check that there isn't any duplicate names *)
	let (_, dups) = pattern_dup pat StringSet.empty StringSet.empty in
	if dups <> StringSet.empty
	then raise (MultiBind (false, range_empty, dups)) else

	match (pat, value) with

	(* Wildcards and identifiers always match *)
	| P_Wildcard, _ -> []
	| P_Name i, _ -> [ (i, value) ]

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

(* pattern_bind [pattern -> value -> env -> env]
   A higher-level interface to pattern_unif. Matches the given value against
   the pattern, and extends the environment with the resulting bindings *)
let pattern_bind pat value env : env =
	(* Get a list of bindings by term unification *)
	let bindings = pattern_unify pat value in
	(* Perform the bindings in order *)
	let bind env (name, value) = {
		env with vars = StringMap.add name value env.vars
	} in
	List.fold_left bind env bindings
