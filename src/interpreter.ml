(*
**	Interpreter - The higher-level interface for program execution
**	This is where statements are evaluated and where language-related
**	decisions, type inference and interaction with the shell happen.
*)

open Types
open Expr
open Pattern
open Exceptions

(* interpreter_start [unit -> env]
   Creates a new execution environment for a program *)
let interpreter_start () =
	let env = {
		vars  = StringMap.empty;
		types = StringMap.empty;
	}
	in env

(* interpreter_exec [statement -> env -> env * event list]
   Executes a statement and returns the updated environment along with a list
   of events, for post-processing (such as showing in terminal) *)
let interpreter_exec stmt env = match stmt with

	(* For raw expressions, just yield the result *)
	| S_Expr (_, e) ->
		(env, [Ev_Result (expr_eval e env)])

	(* For let-value, return a (possibly empty) list of name bindings if the
	   pattern is not "_", otherwise return a result *)
	| S_LetVal (r, pat, e) ->
		(* unify_bind may throw various exceptions for which it does not have
		   the range: add it on the fly *)
		(* TODO: This would not be needed if patterns had a range *)
		begin try
			let value = expr_eval e env in
			if pat = P_Wildcard then (env, [Ev_Result value]) else

			(* Perform the bindings in order *)
			let bindings = pattern_unify pat value in
			let bind env (name, value) = {
				env with vars = StringMap.add name value env.vars
			} in
			let env' = List.fold_left bind env bindings

			(* Return the list of names *)
			in (env', List.map (fun (n, v) -> Ev_Binding (n, v)) bindings)
		with
		| MatchError (_, p, v)  -> raise (MatchError (r, p, v))
		| MultiBind (b, _, set) -> raise (MultiBind (b, r, set))
		end

	(* Let-rec only bind names, so return a single binding *)
	| S_LetRec (r, n, e) ->
		(* Build an equivalent expression and let expr_eval do the recursion *)
		let name = { range = r; tree = E_Name n } in
		let exp  = { range = r; tree = E_LetRec (n, e, name) } in
		let value = expr_eval exp env in
		({ env with vars = StringMap.add n value env.vars },
		 [Ev_Binding (n, value)])

	(* Type definitions have their own kind of event *)
	(* TODO: Create it *)
	| S_Type (r, name, ctors) ->
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
