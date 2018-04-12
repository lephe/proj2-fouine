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
	| E_Match (e, cl)
	| E_Try (e, cl) ->
		let case_free (p, e) =
			StringSet.diff (expr_free e) (pattern_free p) in
		List.fold_left StringSet.union (expr_free e) (List.map case_free cl)



(*
**	Expression evaluation
**
**	I decided to implement exceptions using a continuation-style exception
**	handler stack, which is only possible if expr_eval is itself written with
**	continuations.
**
**	The following functions are tools used by expr_eval. None of them is
**	exposed by the module's interface.
*)

(* The following functions destruct a value while expecting a specific type.
   They return the constructor argument on success and throw a type error in
   all other cases *)

(* TODO: Do something about the expect_* functions. Typing will make sure that
   TODO: the type is correct, but OCaml will not know... *)

(* expect_int [expr -> env -> (int -> value) -> value] [private] *)
let rec expect_int exp env k =
	eval exp env (function
	| V_Int i -> k i
	| v -> raise (TypeError (exp.range, "int", value_type v env)))

(* expect_closure [expr -> env -> (value StringMap.t -> string option ->
   pattern -> expr -> value) -> value] [private] *)
and expect_closure exp env k =
	eval exp env (function
	| V_Closure (closure, recursive, pat, exp) -> k closure recursive pat exp
	| v -> raise (TypeError (exp.range, "function", value_type v env)))

(* expr_eval [expr -> env -> value]
   Recursively evaluates a single expression within an environment, yielding a
   value (or throwing an exception). The environment argument contains both
   values and types, which makes this function pure. *)
and expr_eval exp env =
	eval exp env (fun v -> v)

(* eval [expr -> env -> (value -> value) -> value]
   Pretty much the same as expr_eval, but in continuation-style! *)
and eval exp env k : value =

	(* This piece of code would automatically show the evaluated expression and
	   a dump of the environment in the terminal. This is very verbose!
	print_string "<<< Evaluating\n";
	range_highlight exp.range stdout;
	EnvMap.iter (fun n v -> Printf.printf "%s: %s\n" n (repr_value v) env.vars;
	print_newline (); *)

	(* A helper for arithmetic operators *)
	let arith op e f k =
		expect_int e env (fun a ->
		expect_int f env (fun b ->
			k (V_Int (op a b))
		)) in

	(* Another one for comparisons *)
	let comp op e f k =
		expect_int e env (fun a ->
		expect_int f env (fun b ->
			k (V_Bool (op a b))
		)) in

	match exp.tree with

	(* Immediately return literals *)
	| E_Int i		-> k (V_Int i)
	| E_Bool b		-> k (V_Bool b)
	| E_Unit		-> k (V_Unit)
	(* Lookup names in the environment *)
	| E_Name n ->
		begin try k (StringMap.find n env.vars) with
		| Not_found -> raise (NameError (exp.range, n))
		end

	(* Type constructors : first check that the constructor exists *)
	| E_Ctor (ctor, e) ->
		if not (StringMap.exists (fun key v -> key = ctor) env.types)
		then raise (NameError (exp.range, ctor))
		else eval e env (fun v -> k (V_Ctor (ctor, v)))

	(* Pattern matching *)
	| E_Match (e, cl) ->
		eval e env (fun v ->

		(* Try to bind the argument to all patterns, in order *)
		let rec try_cases cl v = match cl with
		| [] -> raise (MatchError (exp.range, None, v))
		| (p, f) :: tl ->
			try let newenv = pattern_bind p v env in eval f newenv k with
			| MultiBind (b, _, set) -> raise (MultiBind (b, exp.range, set))
			| MatchError _ -> try_cases tl v in

		try_cases cl v)

	(* Try .. match statements - note that env.exchs contains continuations! *)
	| E_Try (e, cl) ->
		(* Turn the cases into exception handlers. Pretty immediate! Also save
		   the current environment for when an exception is raised *)
		eval e { env with exchs = (env, cl) :: env.exchs } k

	(* Let-value: use term unification (rather, filtering) to get the list of
	   all bindings, then extend the environment *)
	| E_LetVal (pat, e, f) ->
		(* pattern_bind may throw various exceptions for which it does not have
		   the range: add it on the fly *)
		(* TODO: This would not be needed if patterns had a range *)
		begin try eval e env (fun v -> eval f (pattern_bind pat v env) k) with
		| MatchError (_, p, v)  -> raise (MatchError (exp.range, p, v))
		| MultiBind (b, _, set) -> raise (MultiBind (b, exp.range, set))
		end

	(* Let-rec: I achieve recursion by setting an option in the Closure object.
	   When it's on, the function is added to the environment before being
	   evaluated. I tried to add it to its closure, but the EnvMap type doesn't
	   support it - unlike "(name, f) :: closure", "EnvMap.add name f closure"
	   is a function call and therefore not allowed in value-let-rec *)
	| E_LetRec (name, ({ tree = E_Function (pat, exp) } as e), f) ->
		(* Build the closure, but don't add "name" to it *)
		let fv = StringSet.diff (expr_free e) (StringSet.singleton name) in

		let extract name closure =
			try StringMap.add name (StringMap.find name env.vars) closure with
			| Not_found -> raise (NameError (e.range, name)) in

		let closure = StringSet.fold extract fv StringMap.empty in
		let vfun = V_Closure (closure, Some name, pat, exp) in

		(* Then, as usual, extend the environment *)
		eval f { env with vars = StringMap.add name vfun env.vars } k

	(* Forbid let-rec of something else than a function *)
	| E_LetRec _ ->
		raise (Error (exp.range, "only functions are allowed in let rec"))

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
		k (V_Closure (closure, None, pat, e))

	(* A special rule for the "simple calls" to function prInt *)
	(* TODO: Add a value type "built-in function" for prInt *)
	| E_Call ({ tree = E_Name "prInt" }, arg) ->
		expect_int arg env (fun i -> Printf.printf "%d\n" i; k (V_Int i))

	(* Another built-in rule for raise *)
	(* TODO: Really, make it a value *)
	| E_Call ({ tree = E_Name "raise" }, arg) ->

		(* Just like match, unfold all the patterns until one that catches the
		   exception is found. We have a list of lists; each sublist represents
		   the various exception-patterns of a nested try statement *)
		let rec try_handlers handlers v = match handlers with
		| []
			-> raise (UncaughtException (exp.range, v))
		| (env, []) :: other_trys
			-> try_handlers other_trys v
		| (local_env, (p, f) :: tl) :: other_trys ->
			(* Unwind the stack, get the environment where try is being run *)
			try let newenv = pattern_bind p v local_env in
			(* There! We completely drop k and use expr_eval instead *)
			expr_eval f newenv with
			| MultiBind (b, _, set) -> raise (MultiBind (b, exp.range, set))
			| MatchError _ -> try_handlers ((local_env, tl) :: other_trys) v in

		eval arg env (fun v -> try_handlers env.exchs v)

	(* Evaluate and bind the argument, *then* evaluate the function *)
	| E_Call (func, arg) ->
		eval arg  env (fun varg ->
		eval func env (fun vfun ->

		begin try match vfun with

		(* Add the function to the environment if it's recursive *)
		| V_Closure (closure, Some name, pat, exp) ->
			let env = { env with vars = StringMap.add name vfun closure } in
			eval exp (pattern_bind pat varg env) k

		| V_Closure (closure, None, pat, exp) ->
			let env = { env with vars = closure } in
			eval exp (pattern_bind pat varg env) k

		(* Reject values... *)
		| _ -> raise (TypeError (func.range, "function", value_type vfun env))
		with
		| MatchError (_, p, v)  -> raise (MatchError (exp.range, p, v))
		| MultiBind (b, _, set) -> raise (MultiBind (b, exp.range, set))
		end

		))

	(* Conditions - straightforward, type checking happens here (although not
		required by fouine) *)
	| E_If (e, t, f) ->
		eval e env (function
		| V_Bool b -> eval (if b then t else f) env k
		| v -> raise (TypeError (e.range, "bool", value_type v env)))

	(* References - note that Assign returns V_Unit *)
	| E_Ref e ->
		eval e env (fun v -> k (V_Ref (memory_store v)))
	| E_Bang e ->
		eval e env (function
		| V_Ref addr -> k (memory_get addr)
		| v -> raise (TypeError (e.range, "reference", value_type v env)))
	| E_Assign (e, f) ->
		eval e env (function
		| V_Ref addr -> eval f env (fun v -> memory_update addr v; k V_Unit)
		| v -> raise (TypeError (e.range, "reference", value_type v env)))

	(* Tuples
	| E_Tuple l -> V_Tuple (List.map (fun x -> eval x env) l) *)
	| E_Tuple l ->
		(* [expr list -> value list -> (value list -> value) -> value] *)
		let rec eval_list l acc k = match l with
		| [] -> k (List.rev acc)
		| e :: tl -> eval e env (fun r -> eval_list tl (r :: acc) k) in
		eval_list l [] (fun vl -> k (V_Tuple vl))

	(* Arithmetic operations *)
	| E_UPlus  e	-> expect_int e env (fun i -> k (V_Int i))
	| E_UMinus e	-> expect_int e env (fun i -> k (V_Int (-i)))
	| E_Plus		(e, f) -> arith (+) e f k
	| E_Minus		(e, f) -> arith (-) e f k
	| E_Times		(e, f) -> arith ( * ) e f k
	| E_Divide		(e, f) ->
		expect_int e env (fun a ->
		expect_int f env (fun b ->
			if b = 0 then raise (ZeroDivision f.range) else k (V_Int (a / b))
		))

	(* Comparisons *)
	| E_Equal			(e, f) -> comp (=)  e f k
	| E_NotEqual		(e, f) -> comp (<>) e f k
	| E_Greater			(e, f) -> comp (>)  e f k
	| E_GreaterEqual	(e, f) -> comp (>=) e f k
	| E_Lower			(e, f) -> comp (<)  e f k
	| E_LowerEqual		(e, f) -> comp (<=) e f k
