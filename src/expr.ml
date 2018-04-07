(*
**	Expr - Expression representation and evaluation
**	Note : the code in this file is not very readable because of the not-so-
**	interesting pattern matching in expr_print and expr_source. I apologize.
*)

open Types
open Pattern
open Exceptions
open Memory
open Value

(* expr_free [expr -> StringSet.t]
   Returns the set of free variables in an expression *)
let rec expr_free exp =
	match exp.tree with

	(* Bases cases with one or no free variable *)
	| E_Int _ | E_Bool _ | E_Unit ->
		StringSet.empty
	| E_Name n ->
		StringSet.of_list [ n ]

	(* Trivial recursion - ADTs and unary operations *)
	| E_Ctor (_, e) | E_Ref e | E_Bang e | E_UPlus e | E_UMinus e ->
		expr_free e

	(* Union of two or more recursive sets *)
	| E_Plus  (e, f) | E_Minus      (e, f) | E_Times   (e, f) | E_Divide (e, f)
	| E_Equal (e, f) | E_NotEqual   (e, f) | E_Greater (e, f)
	| E_Lower (e, f) | E_LowerEqual (e, f) | E_GreaterEqual (e, f)
	| E_Call (e, f)
	| E_Assign (e, f) ->
		StringSet.union (expr_free e) (expr_free f)
	| E_If (e, t, f) ->
		StringSet.union (expr_free e)
		(StringSet.union (expr_free t) (expr_free f))
	| E_Tuple l ->
		List.fold_left StringSet.union StringSet.empty (List.map expr_free l)

	(* Bindings that remove free variables *)
	| E_LetVal (pat, e, f) ->
		let un = StringSet.union (expr_free e) (expr_free f)
		in StringSet.diff un (pattern_free pat)
	| E_LetRec (name, e, f) ->
		let un = StringSet.union (expr_free e) (expr_free f)
		in StringSet.remove name un
	| E_Function (pat, e) ->
		StringSet.diff (expr_free e) (pattern_free pat)

	(* Pattern matching, quite the hardest *)
	| E_Match (e, cl) ->
		let case_free (p, e) =
			StringSet.diff (expr_free e) (pattern_free p) in
		List.fold_left StringSet.union (expr_free e) (List.map case_free cl)



(* The following functions destruct a value while expecting a specific type.
   They return the constructor argument on success and throw a type error in
   all other cases *)

(* TODO: Do something about the expect_* functions. Typing will make sure that
   TODO: the type is correct, but OCaml will not know... *)

let rec expect_int exp env =
	let v = expr_eval exp env in match v with
	| V_Int i -> i
	| _ -> raise (TypeError (exp.range, "int", value_type v env))

and expect_closure exp env =
	let v = expr_eval exp env in match v with
	| V_Closure (closure, recursive, pat, exp) -> (closure, recursive, pat,exp)
	| _ -> raise (TypeError (exp.range, "function", value_type v env))

(* expr_eval [expr -> env -> value]
   Recursively evaluates a single expression within an environment, yielding a
   value (or throwing an exception). The environment argument contains both
   values and types, which makes this function pure *)
and expr_eval exp env : value =

	(* This piece of code would automatically show the evaluated expression and
	   a dump of the environment in the terminal. This is very verbose!
	print_string "<<< Evaluating\n";
	range_highlight exp.range stdout;
	EnvMap.iter (fun n v -> Printf.printf "%s: %s\n" n (repr_value v) env.vars;
	print_newline (); *)

	let eval = expr_eval in

	(* A helper for arithmetic operators *)
	let arith op e f =
		let a = expect_int e env
		and b = expect_int f env in
		V_Int (op a b) in

	(* Another one for comparisons *)
	let comp op e f =
		let a = expect_int e env
		and b = expect_int f env in
		V_Bool (op a b) in

	match exp.tree with

	(* Immediately return literals *)
	| E_Int i		-> V_Int i
	| E_Bool b		-> V_Bool b
	| E_Unit		-> V_Unit
	(* Lookup names in the environment *)
	| E_Name n ->
		begin try StringMap.find n env.vars with
		| Not_found -> raise (NameError (exp.range, n))
		end

	(* Type constructors : first check that the constructor exists *)
	| E_Ctor (ctor, e) ->
		if not (StringMap.exists (fun k v -> k = ctor) env.types)
		then raise (NameError (exp.range, ctor))
		else V_Ctor (ctor, eval e env)

	(* Pattern matching *)
	| E_Match (e, cl) ->
		let v = eval e env in

		(* Try to bind the argument to all patterns, in order *)
		let rec try_cases cl v = match cl with
		| [] -> raise (MatchError (exp.range, None, v))
		| (p, e) :: tl ->
			try let newenv = pattern_bind p v env in eval e newenv with
			| MultiBind (b, _, set) -> raise (MultiBind (b, exp.range, set))
			| MatchError _ -> try_cases tl v in

		try_cases cl v

	(* Let-value: use term unification (rather, filtering) to get the list of
	   all bindings, then extend the environment *)
	| E_LetVal (pat, e, f) ->
		(* pattern_bind may throw various exceptions for which it does not have
		   the range: add it on the fly *)
		(* TODO: This would not be needed if patterns had a range *)
		begin try eval f (pattern_bind pat (eval e env) env) with
		| MatchError (_, p, v)  -> raise (MatchError (exp.range, p, v))
		| MultiBind (b, _, set) -> raise (MultiBind (b, exp.range, set))
		end

	(* Let-rec: I achieve recursion by setting an option in the Closure object.
	   When it's on, the function is added to the environment before being
	   evaluated. I tried to add it to its closure, but the EnvMap type doesn't
	   support it - unlike "(name, f) :: closure", "EnvMap.add name f closure"
	   is a function call and therefore not allowed in value-let-rec *)
	| E_LetRec (name, e, f) ->
		(* Add a dummy polymorphic value for free variable enumeration (see
		   "types.ml" for a few more details *)
		let tmp = { env with vars = StringMap.add name V_Rec env.vars } in

		(* Build the closure using this "V_Rec", and remove it if it's been
		   captured as a free variable. It will be managed on-the-fly *)
		let (closure, _, pat, exp) = expect_closure e tmp in
		let vfun = V_Closure (StringMap.remove name closure, Some name,pat,exp)

		(* Then roll! *)
		in eval f ({ env with vars = StringMap.add name vfun env.vars })

	(* Build function closures by enumerating free variables out of the
	   environment *)
	| E_Function (pat, e) ->
		let freevars = expr_free exp in
		(* Complain if any free variables are not defined *)
		let extract name closure =
			let value = try StringMap.find name env.vars with
			| Not_found -> raise (NameError (e.range, name)) in
			StringMap.add name value closure in
		(* Build the closure and return. Recursion is handled by LetRec *)
		let closure = StringSet.fold extract freevars StringMap.empty in
		V_Closure (closure, None, pat, e)

	(* A special rule for the "simple calls" to function prInt *)
	(* TODO: Add a value type "built-in function" for prInt *)
	| E_Call ({ tree = E_Name "prInt" }, arg) ->
		let i = expect_int arg env in
		print_int i; print_newline (); V_Int i

	(* Evaluate and bind the argument, *then* evaluate the function *)
	| E_Call (func, arg) ->
		let varg = eval arg env in
		let vfun = eval func env in

		begin try match vfun with

		(* Add the function to the environment if it's recursive *)
		| V_Closure (closure, Some name, pat, exp) ->
			let env = {
				vars = StringMap.add name vfun closure;
				types = env.types
			} in
			eval exp (pattern_bind pat varg env)

		| V_Closure (closure, None, pat, exp) ->
			let env = { vars = closure; types = env.types } in
			eval exp (pattern_bind pat varg env)

		(* Reject values... *)
		| _ -> raise (TypeError (func.range, "function", value_type vfun env))
		with
		| MatchError (_, p, v)  -> raise (MatchError (exp.range, p, v))
		| MultiBind (b, _, set) -> raise (MultiBind (b, exp.range, set))
		end

	(* Conditions - straightforward, type checking happens here (although not
		required by fouine) *)
	| E_If (e, t, f) ->
		begin match eval e env with
		| V_Bool b -> if b then eval t env else eval f env
		| v -> raise (TypeError (e.range, "bool", value_type v env))
		end

	(* References - note that Assign returns V_Unit *)
	| E_Ref e -> V_Ref (memory_store (eval e env))
	| E_Bang e ->
		let v = eval e env in begin match v with
		| V_Ref addr -> memory_get addr
		| _ -> raise (TypeError (e.range, "reference", value_type v env))
		end
	| E_Assign (e, f) ->
		let v = eval e env in begin match v with
		| V_Ref addr -> memory_update addr (eval f env); V_Unit
		| _ -> raise (TypeError (e.range, "reference", value_type v env))
		end

	(* Tuples *)
	| E_Tuple l -> V_Tuple (List.map (fun x -> eval x env) l)

	(* Arithmetic operations *)
	| E_UPlus  e	-> let i = expect_int e env in V_Int i
	| E_UMinus e	-> let i = expect_int e env in V_Int (-i)
	| E_Plus		(e, f) -> arith (+) e f
	| E_Minus		(e, f) -> arith (-) e f
	| E_Times		(e, f) -> arith ( * ) e f
	| E_Divide		(e, f) ->
		let a = expect_int e env
		and b = expect_int f env in
		if b = 0 then raise (ZeroDivision f.range)
		else V_Int (a / b)

	(* Comparisons *)
	| E_Equal			(e, f) -> comp (=)  e f
	| E_NotEqual		(e, f) -> comp (<>) e f
	| E_Greater			(e, f) -> comp (>)  e f
	| E_GreaterEqual	(e, f) -> comp (>=) e f
	| E_Lower			(e, f) -> comp (<)  e f
	| E_LowerEqual		(e, f) -> comp (<=) e f
