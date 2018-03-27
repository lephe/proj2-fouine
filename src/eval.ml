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
	| Int _			-> "int"
	| Bool _		-> "bool"
	| Unit			-> "unit"
	| Reference _	-> "ref"
	| Ctor (c, _)	-> StringMap.find c env.types
	| Recursive		-> "<recursion placeholder>"
	| Closure (_, None, _, _) -> "<closure>"
	| Closure (_, Some _,  _, _) -> "<recursive closure>"
	| Tuple l ->
		let f v = value_type v env in
		"(" ^ String.concat " * " (List.map f l) ^ ")"

(* value_str [value -> string]
   Returns a printable (not unambiguous) representation of a value *)
let rec value_str v = match v with
	| Int i			-> string_of_int i
	| Bool b		-> if b then "true" else "false"
	| Unit			-> "()"
	| Reference r	-> "ref " ^ (memory_addr_str r)
	| Ctor (c, e)	-> c ^ " " ^ (value_str e)
	| Recursive		-> "<cycle placeholder>"
	| Closure (e, recursive, pat, t) ->
		if recursive <> None then "<rec closure>" else "<closure>"
	| Tuple l -> "(" ^ String.concat ", " (List.map value_str l) ^ ")"

(* value_print [value -> unit]
   Outputs a description of the value on stdout. This function gives more
   detailed output than value_str on closures *)
let rec value_print v = match v with
	| Int _ | Bool _ | Unit | Reference _ | Ctor _ | Recursive | Tuple _ ->
		print_string (value_str v ^ "\n")
	(* TODO: Recursively show free functions in closure *)
	| Closure (e, recursive, pat, t) ->
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
	| Int i -> i
	| _ -> raise (TypeError (exp.range, "int", value_type v env))

and expect_closure exp env =
	let v = eval exp env in match v with
	| Closure (closure, recursive, pat, exp) -> (closure, recursive, pat, exp)
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
		Int (op a b) in

	(* Another one for comparisons *)
	let comp op e f =
		let a = expect_int e env
		and b = expect_int f env in
		Bool (op a b) in

	match exp.tree with

	(* Immediately return literals, lookup names in the environment *)
	| LiteralInt i	-> Int i
	| LiteralBool b	-> Bool b
	| LiteralUnit	-> Unit
	| Name n ->
		begin try StringMap.find n env.vars with
		| Not_found -> raise (NameError (exp.range, n))
		end

	(* Type declarations - extend the existing constructor set *)
	| TypeDecl (name, ctors, e) ->
		(* Check that constructors are not defined twice *)
		let rec check ctor types =
			if StringMap.mem ctor types
			then raise (TypeOverload (exp.range, ctor)) in

		(* Check that the type name is not already used *)
		if List.exists (fun n -> StringMap.find_opt n env.types = Some name)
			ctors
		then raise (MultiBind (true, exp.range, StringSet.singleton name)) else

		(* Extend the environment with the new constructors *)
		let add types ctor =
			check ctor types;
			StringMap.add ctor name types in
		let newtypes = List.fold_left add env.types ctors in
		eval e { env with types = newtypes }

	(* Type constructors : check that the type exists, then bind *)
	| ExprCtor (ctor, e) ->
		if not (StringMap.exists (fun k v -> k = ctor) env.types)
		then raise (NameError (exp.range, ctor))
		else Ctor (ctor, eval e env)

	(* Use term unification (actually filtering) to get the list of all
	   bindings, then extend the environment *)
	| Let (recursive, pat, e, f) ->
		if not recursive then
			let value = eval e env in
			try eval f (unify_bind pat value env) with
			| MultiBind (b, _, set) -> raise (MultiBind (b, exp.range, set))

		(* I implement recursion by setting a flag in the Closure object. When
		   it's on, the function is added to the environment before being
		   evaluated. I tried to add it to its closure, but the EnvMap type
		   prevents it - unlike "(name, f) :: closure", "EnvMap.add name f
		   closure" is a function call and therefore not allowed in the right-
		   hand side of a let-rec value definition *)

		(* First ensure the pattern has a suitable form *)
		else let name = match pat with
		| Identifier n -> n
		| _ -> raise (Error (exp.range, "This kind of pattern is not " ^
			"allowed inside a recursive binding")) in

		(* Add a dummy polymorphic value for free variable enumeration (see
		   "types.ml" for a few more details *)
		let tmp = { env with vars = StringMap.add name Recursive env.vars } in

		(* Build the closure using this "Recursive", and remove it if it's been
		   captured as a free variable *)
		let (closure, _, pat, exp) = expect_closure e tmp in
		let vfun = Closure (StringMap.remove name closure, Some name, pat, exp)

		(* Then roll! *)
		in eval f ({ env with vars = StringMap.add name vfun env.vars })

	(* For functions - just build the closure by extracting free variables from
	   the environment *)
	| Function (pat, e) ->
		let freevars = expr_free exp in
		(* Complain if the free variables are not defined *)
		let extract name closure =
			let value = try StringMap.find name env.vars with
			| Not_found -> raise (NameError (e.range, name)) in
			StringMap.add name value closure in
		(* Build the function's closure and return. We don't handle recursion,
		   this is a work for Let *)
		let closure = StringSet.fold extract freevars StringMap.empty in
		Closure (closure, None, pat, e)

	(* A special rule for the "simple calls" to function prInt *)
	(* TODO: Add a value type "built-in function" for prInt *)
	| Call ({ tree = Name "prInt" }, arg) ->
		let i = expect_int arg env in
		print_int i; print_newline (); Int i

	(* Evaluate and bind the argument, *then* evaluate the function *)
	| Call (func, arg) ->
		let varg = eval arg env in
		let vfun = eval func env in

		(* Add the function to the environment if it's recursive *)
		begin try match vfun with
		| Closure (closure, Some name, pat, exp) ->
			let env = {
				vars = StringMap.add name vfun closure;
				types = env.types
			} in
			eval exp (unify_bind pat varg env)
		| Closure (closure, None, pat, exp) ->
			let env = { vars = closure; types = env.types } in
			eval exp (unify_bind pat varg env)
		(* Reject values... *)
		| _ -> raise (TypeError (func.range, "function", value_type vfun env))
		with
		| MultiBind (b, _, set) -> raise (MultiBind (b, exp.range, set))
		end

	(* Conditions - straightforward, type checking happens here (although not
	   required by fouine) *)
	| If (e, t, f) ->
		begin match eval e env with
		| Bool b -> if b then eval t env else eval f env
		| v -> raise (TypeError (e.range, "bool", value_type v env))
		end

	(* References - note that Assign returns unit *)
	| Ref e -> Reference (memory_create (eval e env))
	| Bang e ->
		let v = eval e env in begin match v with
		| Reference addr -> memory_get addr
		| _ -> raise (TypeError (e.range, "reference", value_type v env))
		end
	| Assign (e, f) ->
		let v = eval e env in begin match v with
		| Reference addr -> memory_update addr (eval f env); Unit
		| _ -> raise (TypeError (e.range, "reference", value_type v env))
		end

	(* Tuples *)
	| ExprTuple l -> Tuple (List.map (fun x -> eval x env) l)

	(* Arithmetic operations *)
	| UPlus  e -> let i = expect_int e env in Int i
	| UMinus e -> let i = expect_int e env in Int (-i)
	| Plus		(e, f) -> arith (+) e f
	| Minus		(e, f) -> arith (-) e f
	| Times		(e, f) -> arith ( * ) e f
	| Divide	(e, f) ->
		let a = expect_int e env
		and b = expect_int f env in
		if b = 0 then raise (ZeroDivision f.range)
		else Int (a / b)

	(* Comparisons *)
	| Equal			(e, f) -> comp (=)  e f
	| NotEqual		(e, f) -> comp (<>) e f
	| Greater		(e, f) -> comp (>)  e f
	| GreaterEqual	(e, f) -> comp (>=) e f
	| Lower			(e, f) -> comp (<)  e f
	| LowerEqual	(e, f) -> comp (<=) e f
