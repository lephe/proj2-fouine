(*
**	Expr - Expression representation and evaluation
**  Note : the code in this file is not very readable because of the not-so-
**  interesting pattern matching in expr_print and expr_source. I apologize.
*)

open Types
open Util
open Pattern

(*
	Utility functions and expression manipulation
	TODO: Enhance the way expression-printing functions are written
*)

(* expr_free [expr -> StringSet.t]
   Returns the set of all free variables in the given expression *)
let rec expr_free exp =
	match exp.tree with

	(* Bases cases with one or no free variable *)
	| E_Int _ | E_Bool _ | E_Unit ->
		StringSet.empty
	| E_Name n ->
		StringSet.of_list [ n ]

	(* Trivial recursion - ADTs and unary "operators" *)
	| E_Ctor (_, e) | Ref e | Bang e | UPlus e | UMinus e ->
		expr_free e

	(* Union of two recursive sets *)
	| Plus (e, f) | Minus (e, f) | Times (e, f) | Divide (e, f)
	| Equal (e, f) | NotEqual (e, f) | Greater (e, f)
	| GreaterEqual (e, f) | Lower (e, f) | LowerEqual (e, f)
	| Call (e, f)
	| Assign (e, f) ->
		StringSet.union (expr_free e) (expr_free f)

	(* Compound expressions *)
	| If (e, t, f) ->
		StringSet.union (expr_free e)
		(StringSet.union (expr_free t) (expr_free f))
	| E_LetVal (pat, e, f) ->
		let un = StringSet.union (expr_free e) (expr_free f)
		in StringSet.diff un (pattern_free pat)
	| E_LetRec (name, e, f) ->
		let un = StringSet.union (expr_free e) (expr_free f)
		in StringSet.remove name un

	(* Pattern matching is the hardest *)
	| Match (e, cl) ->
		let case_free (p, e) =
			StringSet.diff (expr_free e) (pattern_free p) in
		List.fold_left StringSet.union (expr_free e) (List.map case_free cl)

	(* Functions *)
	| Function (pat, e) ->
		StringSet.diff (expr_free e) (pattern_free pat)

	(* Tuples *)
	| E_Tuple l ->
		List.fold_left StringSet.union StringSet.empty (List.map expr_free l)

(* expr_print [int -> expr -> unit]
   Display the syntax tree of an expression on stdout, indenting each line with
   the provided level of indent. This function assumes that it's called at the
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

	| E_Int i	-> Printf.printf "%d\n" i
	| E_Bool b	-> print_string (if b then "true\n" else "false\n")
	| E_Unit	-> print_string "()\n"
	| E_Name n	-> Printf.printf "{%s}\n" n

	| E_Ctor (ctor, e) -> recurse ctor [e]

	| Match (e, cl) ->
		recurse "match" [e];
		List.iter (fun (p, e) ->
			space ();
			recurse (pattern_str p ^ " -> ") [e]
		) cl

	| E_LetVal (pat, e, f) ->
		recurse ("let " ^ pattern_str pat ^ " = .. in") [e; f]
	| E_LetRec (name, e, f) ->
		recurse ("let rec " ^ name ^ " = .. in") [e; f]

	| If (e, t, f) ->
		if f.tree = E_Unit then recurse "if-then" [e; t]
		else recurse "if-then-else" [e; t; f]

	| Function (pat, e) ->
		recurse ("fun " ^ pattern_str pat ^ " ->") [e]
	| Call (f, v) -> recurse "call" [f; v]

	| Bang e		-> recurse "!" [e]
	| Ref e 		-> recurse "ref" [e]
	| Assign (e, f)	-> recurse ":=" [e; f]

	| E_Tuple l		-> recurse "tuple" l

	| UPlus  e		-> recurse "+/1" [e]
	| UMinus e		-> recurse "-/1" [e]

	| Plus			(e, f) -> recurse "+" [e; f]
	| Minus			(e, f) -> recurse "-" [e; f]
	| Times			(e, f) -> recurse "*" [e; f]
	| Divide		(e, f) -> recurse "/" [e; f]

	| Equal			(e, f) -> recurse "="  [e; f]
	| NotEqual		(e, f) -> recurse "<>" [e; f]
	| Greater		(e, f) -> recurse ">"  [e; f]
	| GreaterEqual	(e, f) -> recurse ">=" [e; f]
	| Lower			(e, f) -> recurse "<"  [e; f]
	| LowerEqual	(e, f) -> recurse "<=" [e; f]

(* expr_source2 [int -> expr -> unit] [private]
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
	| E_Int  i	-> print_int i
	| E_Bool b	-> print_string (if b then "true" else "false")
	| E_Unit	-> print_string "()"
	| E_Name n	-> print_string n

	| E_Ctor (ctor, e) ->
		Printf.printf "%s " ctor;
		expr_source2 level e

	| Match (e, cl) ->
		print_string "(match ";
		expr_source2 level e;
		print_string " with ";
		List.iter (fun (p, e) ->
			print_string "\n";
			space (level + 1);
			Printf.printf "| %s -> " (pattern_str p);
			expr_source2 (level + 1) e
		) cl;
		print_string ")";

	| E_LetVal (pat, e, f) ->
		print_string ("let " ^ pattern_str pat ^ " = ");
		expr_source2 level e;
		print_string " in\n";
		space level;
		expr_source2 level f

	| E_LetRec (name, e, f) ->
		print_string ("let rec " ^ name ^ " = ");
		expr_source2 level e;
		print_string " in\n";
		space level;
		expr_source2 level f

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
		print_string ("(fun " ^ pattern_str pat ^ " ->\n");
		space (level + 1);
		expr_source2 (level + 1) e;
		print_string ")"
	| Call (f, arg) ->
		expr_source2 level f;
		print_string " (";
		expr_source2 level arg;
		print_string ")"

	(* Reference-related *)
	| Bang e ->
		print_string "!(";
		expr_source2 level e;
		print_string ")"
	| Ref arg ->
		print_string "ref (";
		expr_source2 level arg;
		print_string ")"
	| Assign (e, f) -> binary ":=" e f

	(* Tuples - normally no tuple should be empty... *)
	| E_Tuple [] ->
		print_string "<empty tuple?!>"
	| E_Tuple (hd :: tl) ->
		print_string "(";
		expr_source2 level hd;
		List.iter (fun x -> print_string ","; expr_source2 level x) tl;
		print_string ")"

	(* Unary arithmetic operators *)
	| UPlus e ->
		print_string "+(";
		expr_source2 level e;
		print_string ")"
	| UMinus e ->
		print_string "-(";
		expr_source2 level e;
		print_string ")"

	(* Binary arithmetic operators *)
	| Plus		(e, f) -> binary "+" e f
	| Minus		(e, f) -> binary "-" e f
	| Times		(e, f) -> binary "*" e f
	| Divide	(e, f) -> binary "/" e f

	(* Comparison operators *)
	| Equal			(e, f) -> binary "="  e f
	| NotEqual		(e, f) -> binary "<>" e f
	| Greater		(e, f) -> binary ">"  e f
	| GreaterEqual	(e, f) -> binary ">=" e f
	| Lower			(e, f) -> binary "<"  e f
	| LowerEqual	(e, f) -> binary "<=" e f


(* expr_source [int -> exp -> unit]
   Prints a human-readable listing of a provided expression on stdout *)
let expr_source level exp =
	expr_source2 level exp;
	print_newline ()
