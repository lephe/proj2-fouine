(*
**	Eval - Expression evaluation
*)

open Types
open Expr
open Exceptions
open Pattern
open Memory

(*
	Value functions
*)

(* value_type [value -> env -> string]
   A textual representation of a value's type, withing a given environment *)
let rec value_type v env = match v with
	| V_Int _		-> "int"
	| V_Bool _		-> "bool"
	| V_Unit		-> "unit"
	| V_Ref _		-> "ref"
	| V_Ctor (c, _)	-> StringMap.find c env.types
	| V_Rec			-> "<recursion placeholder>"
	| V_Closure (_, None, _, _) -> "<closure>"
	| V_Closure (_, Some _,  _, _) -> "<recursive closure>"
	| V_Tuple l ->
		let f v = value_type v env in
		"(" ^ String.concat " * " (List.map f l) ^ ")"

(* value_str [value -> string]
   Returns a printable (not unambiguous) representation of a value *)
let rec value_str v = match v with
	| V_Int i		-> string_of_int i
	| V_Bool b		-> if b then "true" else "false"
	| V_Unit		-> "()"
	| V_Ref r		-> "ref " ^ (memory_addr_str r)
	| V_Ctor (c, e)	-> c ^ " " ^ (value_str e)
	| V_Rec			-> "<cycle placeholder>"
	| V_Closure (e, recursive, pat, t) ->
		if recursive <> None then "<rec closure>" else "<closure>"
	| V_Tuple l -> "(" ^ String.concat ", " (List.map value_str l) ^ ")"

(* value_print [value -> unit]
   Outputs a description of the value on stdout. This function gives more
   detailed output than value_str on closures *)
let rec value_print v = match v with
	| V_Int _ | V_Bool _ | V_Unit | V_Ref _ | V_Ctor _ | V_Rec | V_Tuple _ ->
		print_string (value_str v ^ "\n")
	(* TODO: Recursively show free functions in closure *)
	| V_Closure (e, recursive, pat, t) ->
		let r = if recursive <> None then "(rec) " else "" in
		print_string (r ^ "fun " ^ pattern_str pat ^ " ->\n"); expr_print 0 t

(*
	Evaluator
*)

(* unify_bind [pattern -> value -> env] [private]
   Match a value against a pattern, and extend the environment with the
   resulting bindings *)
let unify_bind pat value env : env =
	(* Get a list of bindings by term unification *)
	let bindings = pattern_unify pat value in
	(* Perform the bindings in order *)
	let bind env (name, value) = {
		env with vars = StringMap.add name value env.vars
	} in
	List.fold_left bind env bindings

(* "expect" functions - Destruct values and expect types *)

let rec expect_int exp env =
	let v = eval exp env in match v with
	| V_Int i -> i
	| _ -> raise (TypeError (exp.range, "int", value_type v env))

and expect_closure exp env =
	let v = eval exp env in match v with
	| V_Closure (closure, recursive, pat, exp) -> (closure, recursive, pat,exp)
	| _ -> raise (TypeError (exp.range, "function", value_type v env))

(* eval [expr -> env -> value]
   Evaluates an epxression to a value. The second argument contains both the
   currently defined values, and the currently defined type constructors (since
   there is no typing, constructor-type mappings only matter when checking the
   exhaustiveness of match patterns *)
and eval exp env : value =

	(* This piece of code would automatically show the evaluated expression and
	   a dump of the environment in the terminal. This is very verbose!
	print_string "<<< Evaluating\n";
	range_highlight exp.range stdout;
	EnvMap.iter (fun n v->Printf.printf "\"%s\" -> " n;value_print v) env.vars;
	print_newline (); *)

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

	(* Immediately return literals, lookup names in the environment *)
	| E_Int i		-> V_Int i
	| E_Bool b		-> V_Bool b
	| E_Unit		-> V_Unit
	| E_Name n ->
		begin try StringMap.find n env.vars with
		| Not_found -> raise (NameError (exp.range, n))
		end

	(* Type constructors : check that the type exists, then bind *)
	| E_Ctor (ctor, e) ->
		if not (StringMap.exists (fun k v -> k = ctor) env.types)
		then raise (NameError (exp.range, ctor))
		else V_Ctor (ctor, eval e env)

	(* Pattern matching *)
	| Match (e, cl) ->
		let v = eval e env in

		(* Try all cases in order *)
		let rec try_cases cl v = match cl with
		| [] -> raise (MatchError (exp.range, None, v))
		| (p, e) :: tl ->
			try let newenv = unify_bind p v env in eval e newenv with
			| MultiBind (b, _, set) -> raise (MultiBind (b, exp.range, set))
			| MatchError _ -> try_cases tl v in

		try_cases cl v

	(* Let-value: use term unification (rather, filtering) to get the list of
	   all bindings, then extend the environment *)
	| E_LetVal (pat, e, f) ->
		(* unify_bind may throw various exceptions for which it does not have
		   the range: add it on the fly *)
		(* TODO: This would not be needed if patterns had a range *)
		begin try eval f (unify_bind pat (eval e env) env) with
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

		(* Build the closure using this "Recursive", and remove it if it's been
		   captured as a free variable. It will be managed on-the-fly *)
		let (closure, _, pat, exp) = expect_closure e tmp in
		let vfun = V_Closure (StringMap.remove name closure, Some name,pat,exp)

		(* Then roll! *)
		in eval f ({ env with vars = StringMap.add name vfun env.vars })

	(* Build function closures by enumerating free variables out of the
	   environment *)
	| Function (pat, e) ->
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
	| Call ({ tree = E_Name "prInt" }, arg) ->
		let i = expect_int arg env in
		print_int i; print_newline (); V_Int i

	(* Evaluate and bind the argument, *then* evaluate the function *)
	| Call (func, arg) ->
		let varg = eval arg env in
		let vfun = eval func env in

		begin try match vfun with

		(* Add the function to the environment if it's recursive *)
		| V_Closure (closure, Some name, pat, exp) ->
			let env = {
				vars = StringMap.add name vfun closure;
				types = env.types
			} in
			eval exp (unify_bind pat varg env)

		| V_Closure (closure, None, pat, exp) ->
			let env = { vars = closure; types = env.types } in
			eval exp (unify_bind pat varg env)

		(* Reject values... *)
		| _ -> raise (TypeError (func.range, "function", value_type vfun env))
		with
		| MatchError (_, p, v)  -> raise (MatchError (exp.range, p, v))
		| MultiBind (b, _, set) -> raise (MultiBind (b, exp.range, set))
		end

	(* Conditions - straightforward, type checking happens here (although not
		required by fouine) *)
	| If (e, t, f) ->
		begin match eval e env with
		| V_Bool b -> if b then eval t env else eval f env
		| v -> raise (TypeError (e.range, "bool", value_type v env))
		end

	(* References - note that Assign returns unit *)
	| Ref e -> V_Ref (memory_create (eval e env))
	| Bang e ->
		let v = eval e env in begin match v with
		| V_Ref addr -> memory_get addr
		| _ -> raise (TypeError (e.range, "reference", value_type v env))
		end
	| Assign (e, f) ->
		let v = eval e env in begin match v with
		| V_Ref addr -> memory_update addr (eval f env); V_Unit
		| _ -> raise (TypeError (e.range, "reference", value_type v env))
		end

	(* Tuples *)
	| E_Tuple l -> V_Tuple (List.map (fun x -> eval x env) l)

	(* Arithmetic operations *)
	| UPlus  e -> let i = expect_int e env in V_Int i
	| UMinus e -> let i = expect_int e env in V_Int (-i)
	| Plus		(e, f) -> arith (+) e f
	| Minus		(e, f) -> arith (-) e f
	| Times		(e, f) -> arith ( * ) e f
	| Divide	(e, f) ->
		let a = expect_int e env
		and b = expect_int f env in
		if b = 0 then raise (ZeroDivision f.range)
		else V_Int (a / b)

	(* Comparisons *)
	| Equal			(e, f) -> comp (=)  e f
	| NotEqual		(e, f) -> comp (<>) e f
	| Greater		(e, f) -> comp (>)  e f
	| GreaterEqual	(e, f) -> comp (>=) e f
	| Lower			(e, f) -> comp (<)  e f
	| LowerEqual	(e, f) -> comp (<=) e f
