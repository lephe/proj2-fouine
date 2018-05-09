(*
**	Interpreter - The higher-level interface for program execution
**	This is where statements are evaluated and where language-related
**	decisions, type inference and interaction with the shell happen.
*)

open Types
open Expr
open Pattern
open Exceptions
open Value
open Range

(*
**	Built-in functions
**	All builtins have type range -> value -> env -> value as extensions of
**	eval, and take care of unpacking. The range parameter allows them to raise
**	exceptions.
*)

open Repr
open Source

(* builtin_prInt [private]
   [range -> value -> env -> (value -> value) -> value] *)
let builtin_prInt r value env k = match value with
	| V_Int i -> Printf.printf "%d\n" i; k (V_Int i)
	| v -> raise (TypeError (r, "int", value_type v env))

(* builtin_raise [private]
   [range -> value -> env -> (value -> value) -> value] *)
let builtin_raise r value env k =
	(* Unfold all patterns until a suitable exception handler is found. We have
	   a list (nested try stack) of lists (patterns of a single try) *)
	let rec try_handlers handlers v = match handlers with
		(* If all the handlers have been unwound, call the toplevel *)
		| [] -> raise (UncaughtException (r, v))
		(* Unwind the try stack further *)
		| (env, recs, k, []) :: trys -> try_handlers trys v
		(* Try to match the given pattern against the thrown value *)
		| (local_env, local_recs, local_k, (p, f) :: tl) :: trys ->
			(* Use the environment where try is being run - this is crucial! *)
			try let newenv = pattern_bind p v local_env in
			(* There! We completely drop the continuation and use local_k *)
			expr_eval_k f newenv local_recs local_k with
			| MultiBind (b, _, set) -> raise (MultiBind (b, r, set))
			| MatchError _ ->
				try_handlers ((local_env, local_recs, local_k, tl) :: trys) v

	in try_handlers env.exchs value

(* builtin_alloc [private]
   [range -> value -> env -> (value -> value) -> value] *)
let builtin_alloc r value env k = match value with
	| V_Tuple [V_Memory (addr, m); v] ->
		Hashtbl.add m addr v;
		k (V_Tuple [V_Ref addr; V_Memory (addr + 1, m)])
	| v -> raise (TypeError (r, "<builtin memory> * 'a", value_type v env))

(* builtin_read [private]
   [range -> value -> env -> (value -> value) -> value] *)
let builtin_read r value env k = match value with
	| V_Tuple [V_Memory (_, m); V_Ref addr] -> k (Hashtbl.find m addr)
	| v -> raise (TypeError (r, "<builtin memory> * ref", value_type v env))

(* builtin_write [private]
   [range -> value -> env -> (value -> value) -> value] *)
let builtin_write r value env k = match value with
	| V_Tuple [V_Memory (a, m); V_Ref addr; v] ->
		Hashtbl.replace m addr v; k (V_Memory (a, m))
	| v -> raise (TypeError (r,"<builtin memory> * ref * 'a",value_type v env))

(*
**	Interpreter core
*)

(* interpreter_start [char list -> env]
   Creates a new execution environment for a program. The base environment
   depends on the transformations applied on the source code, which may only be
   ['R'], ['E'], ['R';'E'] or ['E';'R'] *)
let interpreter_start tr_list =
	let add map (key, value) = StringMap.add key value map in

	(* Memory used by imperative programs *)
	let s = V_Memory (0, Hashtbl.create 100) in

	(* Other builtins that are specific to transformed programs *)
	let tr_specific = match tr_list with
	| []			-> []
	| ['R']			-> [ ("s", s) ]
	| ['E']			-> []
	| ['R'; 'E']	-> [ ("_s", s) ]
	| ['E'; 'R']	-> [ ("s", s) ]
	| _ -> raise (InternalError "interpreter_start: unknown transformation") in

	let list_type = [
		("Empty",	"list");
		("Cons",	"list");
		("E",		"exn");
	] in

	let builtins  = [
		("prInt", V_Builtin builtin_prInt);
		("raise", V_Builtin builtin_raise);
		("alloc", V_Builtin builtin_alloc);
		("read",  V_Builtin builtin_read);
		("write", V_Builtin builtin_write);
	] in

	let env = {
		vars  = List.fold_left add StringMap.empty (builtins @ tr_specific);
		types = List.fold_left add StringMap.empty list_type;
		exchs = [];
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
	(* TODO: Create the type-definition event (requires structure in types) *)
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
