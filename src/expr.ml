(*
**	Expr - Expression representation and evaluation
*)

open Types
open Util
open Pattern

(*
	Utility functions and expression manipulation
	TODO: Enhance the way expression-printing functions are written
*)

(* expr_free [expr -> StringSet.t]
   Returns the set of all free variables of the given expression *)
let rec expr_free exp = match exp.tree with
	(* Bases cases *)
	| LiteralInt _ | LiteralBool _ | LiteralUnit -> StringSet.empty
	| Name n -> StringSet.of_list [ n ]
	(* Compound expressions *)
	| If (e, t, f) ->
		StringSet.union (expr_free e)
		(StringSet.union (expr_free t) (expr_free f))
	| Let (_, pat, e, f) ->
		let un = StringSet.union (expr_free e) (expr_free f)
		in StringSet.diff un (pattern_free pat)
	| Function (pat, e) ->
		StringSet.diff (expr_free e) (pattern_free pat)
	| Call (f, a) ->
		StringSet.union (expr_free f) (expr_free a)
	(* Boring operators *)
	| UPlus e
	| UMinus e
		-> expr_free e
	| Plus			(e, f)
	| Minus			(e, f)
	| Times			(e, f)
	| Equal			(e, f)
	| NotEqual		(e, f)
	| Greater		(e, f)
	| GreaterEqual	(e, f)
	| Lower			(e, f)
	| LowerEqual	(e, f)
		-> StringSet.union (expr_free e) (expr_free f)

(* expr_print [int -> expr -> unit]
   Display the syntax tree of an expression on stdout, indenting each line with
   the provided level of indent. This function expects to be called at the
   beginning of a line, and prints a final newline *)
let rec expr_print indent exp =

	(* Print 2 * 'indent' spaces, the smart way *)
	let space () =
		Printf.printf "(%s) %*s" (range_str exp.range) (2 * indent) "" in
	space ();

	let recurse (str: string) (children: expr list) : unit =
		print_string (str ^ "\n");
		List.iter (expr_print (indent + 1)) children in

	match exp.tree with

	| LiteralInt i  -> Printf.printf "%d\n" i
	| LiteralBool b -> print_string (if b then "true\n" else "false\n")
	| LiteralUnit   -> print_string "()\n"
	| Name n		-> Printf.printf "{%s}\n" n

	| Let (recursive, pat, e, f) ->
		let key = if recursive then "let rec " else "let " in
		recurse (key ^ pattern_str pat ^ " = ") [e; f]

	| If (e, t, f) ->
		if f.tree = LiteralUnit then recurse "if-then" [e; t]
		else recurse "if-then-else" [e; t; f]

	| Function (pat, e) ->
		recurse ("fun " ^ pattern_str pat ^ " ->") [e]
	| Call (f, v) -> recurse "call" [f; v]

	| UPlus  e -> recurse "+/1" [e]
	| UMinus e -> recurse "-/1" [e]

	| Plus			(e, f) -> recurse "+" [e; f]
	| Minus			(e, f) -> recurse "-" [e; f]
	| Times			(e, f) -> recurse "*" [e; f]

	| Equal			(e, f) -> recurse "="  [e; f]
	| NotEqual		(e, f) -> recurse "<>" [e; f]
	| Greater		(e, f) -> recurse ">"  [e; f]
	| GreaterEqual	(e, f) -> recurse ">=" [e; f]
	| Lower			(e, f) -> recurse "<"  [e; f]
	| LowerEqual	(e, f) -> recurse "<=" [e; f]


(* expr_source2 [int -> expr -> unit] [private function]
   Produce a human-readable listing of an expression
   TODO: Avoid some parentheses by performing priority analysis *)
let rec expr_source2 level exp =

	let space level =
		(* Print 2 * 'level' spaces, the smart way *)
		Printf.printf "%*s" (2 * level) "" in

	let binary op e f =
		print_string "(";
		expr_source2 level e;
		print_string (") " ^ op ^ " (");
		expr_source2 level f;
		print_string ")" in

	match exp.tree with

	(* Simple cases *)
	| LiteralInt  i	-> print_int i
	| LiteralBool b	-> print_string (if b then "true" else "false")
	| LiteralUnit	-> print_string "()"
	| Name n		-> print_string n

	(* Composed expressions. For lets, only increase indent when there are
	   parameters (ie. function definition vs variable binding) *)
	| Let (recursive, pat, e, f) ->
		let key = if recursive then "let rec " else "let " in
		print_string (key ^ pattern_str pat ^ " = ");
		expr_source2 level e;
		print_string " in\n";
		(* TODO: Figure this out *)
		(* let newlevel = if snd binding != [] then level + 1 else level in *)
		let newlevel = level in
		space newlevel;
		expr_source2 newlevel f

	| If (e, t, f) ->
		print_string "if ";
		expr_source2 level e;
		print_string " then\n";
		space (level + 1);
		expr_source2 (level + 1) t;
		print_string "\n";
		space level;
		print_string "else\n";
		space (level + 1);
		expr_source2 (level + 1) f

	(* Functional expressions *)
	| Function (pat, e) ->
		print_string ("fun " ^ pattern_str pat ^ " ->\n");
		space (level + 1);
		expr_source2 (level + 1) e
	| Call (f, arg) ->
		expr_source2 level f;
		print_string " (";
		expr_source2 level arg;
		print_string ")"

	(* Unary arithmetic operators *)
	| UPlus e ->
		print_string "+";
		expr_source2 level e
	| UMinus e ->
		print_string "-";
		expr_source2 level e

	(* Binary arithmetic operators *)
	| Plus (e, f)  -> binary "+" e f
	| Minus (e, f) -> binary "-" e f
	| Times (e, f) -> binary "*" e f

	(* Comparison operators *)
	| Equal			(e, f) -> binary "="  e f
	| NotEqual		(e, f) -> binary "<>" e f
	| Greater		(e, f) -> binary ">"  e f
	| GreaterEqual	(e, f) -> binary ">=" e f
	| Lower			(e, f) -> binary "<"  e f
	| LowerEqual	(e, f) -> binary "<=" e f

(* expr_source [int -> exp -> unit]
   Produce a human-readable listing of a provided expression *)
let expr_source level exp =
	expr_source2 level exp;
	print_newline ()
