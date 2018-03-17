(*
**	Eval - Expression evaluation
*)

open Types
open Expr
open Exceptions
open Pattern

(*
	Value functions
*)

(* value_type - Get a value's typename *)
let value_type v = match v with
	| Int	_ -> "int"
	| Bool	_ -> "bool"
	| Unit    -> "unit"
	| Closure (_, _, _) -> "<closure>"

(* value_print - Just print values *)
let rec value_print v = match v with
	| Int i		-> print_int i; print_string "\n"
	| Bool b	-> print_string (if b then "true\n" else "false\n")
	| Unit 		-> print_string "()\n"
	(* TODO: Recursively show free functions in closure *)
	| Closure (e, pat, t) ->
		print_string ("fun " ^ pattern_str pat ^ " ->\n"); expr_print 0 t

(*
	Evaluator
*)

(* unify_bind - Unify a pattern with a value, and bind the result *)
let unify_bind pat value env : env =
	(* Get a list of bindings by term unification *)
	let bindings = pattern_unify pat value in
(*	List.iter (fun (name, value) ->
		Printf.printf "Binding %s to " name;
		value_print value)
		bindings;
	print_newline (); *)
	(* Perform the bindings in order *)
	let bind env (name, value) = EnvMap.add name value env in
	List.fold_left bind env bindings


(* "expect" functions - Destruct values and expect types *)

let rec expect_int exp env =
	let v = eval exp env in match v with
	| Int i -> i
	| _ -> raise (TypeError (exp.range, "int", value_type v))

and expect_bool exp env =
	let v = eval exp env in match v with
	| Bool b -> b
	| _ -> raise (TypeError (exp.range, "bool", value_type v))

and expect_unit exp env =
	let v = eval exp env in match v with
	| Unit -> ()
	| _ -> raise (TypeError (exp.range, "unit", value_type v))

and expect_closure exp env =
	let v = eval exp env in match v with
	| Closure (closure, pat, exp) -> (closure, pat, exp)
	| _ -> raise (TypeError (exp.range, "function", value_type v))

and eval exp env : value =

	(* A helper for arithmetic operators *)
	let arith op e f =
		let a = expect_int e env
		and b = expect_int f env in
		Int (op a b) in

	(* Another one for comparisons *)
	let comp op e f =
		let a = expect_int e env
		and b = expect_int f env in
		Bool (op a b) in

	match exp.tree with
	| LiteralInt i	-> Int i
	| LiteralBool b	-> Bool b
	| LiteralUnit	-> Unit

	| Name n ->
		begin try EnvMap.find n env with
		| Not_found -> raise (NameError (exp.range, n))
		end

	| Let (recursive, pat, e, f) ->
		if recursive then raise (InternalError "TODO: recursive bindings x_x");
		(* Use term unification to get the list of all bindings, then add them
		   to the current environment *)
		let value = eval e env in
		eval f (unify_bind pat value env)

	| Function (pat, e) ->
		(* Extract the free variables from the environment... or complain *)
		let freevars = expr_free exp in
		let extract name closure =
			let value = try EnvMap.find name env with
			| Not_found -> raise (NameError (e.range, name)) in
			EnvMap.add name value closure in
		(* Build the function's closure and return *)
		let closure = StringSet.fold extract freevars EnvMap.empty in
		Closure (closure, pat, e)

	(* A special rule for the "simple calls" to function prInt *)
	(* TODO: Add a value type "built-in function" for prInt *)
	| Call ({ tree = Name "prInt" }, arg) ->
		let i = expect_int arg env in
		print_int i; print_newline (); Int i

	| Call (func, arg) ->
		(* First evaluate the argument, then the function. This is crucial! *)
		let varg = eval arg env in
		(* Check that the left-hand side is indeed a function *)
		let (closure, pat, exp) = expect_closure func env in

		(* Bind the arguments to the environment provided by the closure, and
		   evaluate the function body within the resulting environment *)
		eval exp (unify_bind pat varg closure)

	| If (e, t, f) ->
		let b = expect_bool e env in
		if b then eval t env else eval f env

	(* Arithmetic operations *)
	| UPlus  e -> let i = expect_int e env in Int i
	| UMinus e -> let i = expect_int e env in Int (-i)
	| Plus		(e, f) -> arith (+) e f
	| Minus		(e, f) -> arith (-) e f
	| Times		(e, f) -> arith ( * ) e f

	(* Comparisons *)
	| Equal			(e, f) -> comp (=)  e f
	| NotEqual		(e, f) -> comp (<>) e f
	| Greater		(e, f) -> comp (>)  e f
	| GreaterEqual	(e, f) -> comp (>=) e f
	| Lower			(e, f) -> comp (<)  e f
	| LowerEqual	(e, f) -> comp (<=) e f

