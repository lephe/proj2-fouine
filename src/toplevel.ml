(*
**	Toplevel - The higher aspects of the Fouine language
**	This is the Fouine toplevel: it orchestrates the program's execution, type
**	inference and interaction with the shell.
*)

open Types
open Expr
open Util
open Pattern
open Eval
open Exceptions

(* decl_print [decl -> unit]
   Print a toplevel declaration on stdout *)
let decl_print d =
	begin match d with
	| D_Expr (_, e) ->
		expr_print 0 e
	| D_LetVal (r, p, e) ->
		Printf.printf "(%s) let %s =\n" (range_str r) (pattern_str p);
		expr_print 1 e
	| D_LetRec (r, n, e) ->
		Printf.printf "(%s) let rec %s =\n" (range_str r) n;
		expr_print 1 e
	| D_Type (r, name, ctors) ->
		Printf.printf "(%s) type %s\n" (range_str r) name;
		List.iter (fun c ->
			Printf.printf "(%s)   %s\n" (range_str r) c
		) ctors
	end; print_string ";;\n"

(* decl_source [decl -> unit]
   Prints a reconstituted source for a toplevel declaration, on stdout *)
let decl_source d =
	begin match d with
	| D_Expr (_, e) ->
		expr_source 0 e
	| D_LetVal (_, p, e) ->
		Printf.printf "let %s = " (pattern_str p); expr_source 0 e
	| D_LetRec (_, n, e) ->
		Printf.printf "let rec %s = " n; expr_source 0 e
	| D_Type (_, name, ctors) ->
		Printf.printf "type %s = %s\n" name (String.concat " | " ctors)
	end; print_string ";;\n"

(* decl_eval [decl -> env -> env * (name * value) list]
   Evaluates a single toplevel declaration and returns the extended environment
   and the result (if any). No result is returned for type definitions *)
let decl_eval d env = match d with
	| D_Expr (_, e) ->
		(env, ["-", eval e env])

	| D_LetVal (r, pat, e) ->
		(* unify_bind may throw various exceptions for which it does not have
		   the range: add it on the fly *)
		(* TODO: This would not be needed if patterns had a range *)
		begin try
			let bindings = pattern_unify pat (eval e env) in

			(* Perform the bindings in order *)
			let bind env (name, value) = {
				env with vars = StringMap.add name value env.vars
			} in
			let env' = List.fold_left bind env bindings

			in (env', bindings)
		with
		| MatchError (_, p, v)  -> raise (MatchError (r, p, v))
		| MultiBind (b, _, set) -> raise (MultiBind (b, r, set))
		end

	| D_LetRec (r, n, e) ->
		let name = { range = r; tree = E_Name n } in
		let exp  = { range = r; tree = E_LetRec (n, e, name) } in
		let value = eval exp env in
		({ env with vars = StringMap.add n value env.vars }, [n, value])

	| D_Type (r, name, ctors) ->
		(* Check that constructors are not defined twice *)
		let rec check ctor types =
			if StringMap.mem ctor types
			then raise (TypeOverload (r, ctor)) in

		(* Check that the type name is not already used *)
		if List.exists (fun n ->
			try StringMap.find n env.types = name
			with Not_found -> false)
			ctors
		then raise (MultiBind (true, r, StringSet.singleton name)) else

		(* Extend the environment with the new constructors *)
		let add types ctor =
			check ctor types;
			StringMap.add ctor name types in
		let newtypes = List.fold_left add env.types ctors in
		({ env with types = newtypes }, [])

(* top_new [unit -> env]
   Creates and returns a fresh environment (used when execution starts) *)
let top_new () =
	let env = {
		vars  = StringMap.empty;
		types = StringMap.empty;
	}
	in env

(* top_exec [(string * value -> unit) -> (name -> string list -> unit) -> env
     -> decl -> env]
   This function is a wrapper for decl_eval that takes care of extending the
   environment, inferring new types (possibly throwing exceptions) and calling
   back the provided functions when names and types are defined *)
let top_exec call_name call_type env dl =
	let (env', values) = decl_eval dl env in
	List.iter call_name values;
	(* TODO: Callback for type definitions *)
	env'
