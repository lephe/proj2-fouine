(*
**	Source - Re-interpretable representations of objects
*)

open Types
open Repr
open Printf

(* source_expr [exp -> int -> string]
   Prints a human-readable listing of a provided expression on stdout *)
let rec source_expr exp level =
	expr level exp ^ "\n"

(* expr [int -> expr -> unit] [private]
   TODO: Avoid some parentheses by performing priority analysis *)
and expr level exp =

	(* space [int -> string]
	   Returns the requested amount of space characters *)
	let space level =
		sprintf "%*s" level "" in

	(* binary [string -> expr -> expr -> string]
	   Recursion for binary operators *)
	let binary op e f =
		sprintf "(%s) %s (%s)" (expr level e) op (expr level f) in

	(* match_case [pattern * expr -> string] *)
	let match_case (p, e) =
		sprintf "\n%s| %s -> %s"
		(space (level + 1)) (repr_pattern p) (expr (level + 1) e) in

	match exp.tree with

	(* Simple cases *)
	| E_Int  i			-> string_of_int i
	| E_Bool b			-> if b then "true" else "false"
	| E_Unit			-> "()"
	| E_Name n			-> n
	| E_Ctor (ctor, e)	-> ctor ^ " " ^ expr level e

	| E_Match (e, cl) ->
		let match_cases = String.concat "" (List.map match_case cl) in
		sprintf "(match %s with%s)" (expr level e) (match_cases)
	| E_Try (e, cl) ->
		let match_cases = String.concat "" (List.map match_case cl) in
		sprintf "(try %s with%s)" (expr level e) (match_cases)

	| E_LetVal (pat, e, f) ->
		sprintf "let %s = %s in\n%s%s"
		(repr_pattern pat) (expr level e) (space level) (expr level f)
	| E_LetRec (name, e, f) ->
		sprintf "let rec %s = %s in\n%s%s"
		(name) (expr level e) (space level) (expr level f)
	| E_If (e, t, f) ->
		sprintf "if %s then\n%s%s\n%selse\n%s%s"
		(expr level e) (space (level + 1)) (expr (level + 1) t) (space level)
		(space (level + 1)) (expr (level + 1) f)

	(* Functional expressions *)
	| E_Function (pat, e) ->
		sprintf "(fun %s ->\n%s%s)"
		(repr_pattern pat) (space (level + 1)) (expr (level + 1) e)
	| E_Call (f, arg) ->
		sprintf "%s (%s)"
		(expr level f) (expr level arg)

	(* Tuples *)
	| E_Tuple (el) ->
		sprintf "(%s)" (String.concat ", " (List.map (expr level) el))

	(* Reference-related *)
	| E_Bang e			-> sprintf "!(%s)" (expr level e)
	| E_Ref arg			-> sprintf "ref (%s)" (expr level arg)
	| E_Assign (e, f)	-> binary ":=" e f

	(* Unary arithmetic operators *)
	| E_UPlus e			-> sprintf "+(%s)" (expr level e)
	| E_UMinus e		-> sprintf "-(%s)" (expr level e)

	(* Binary arithmetic operators *)
	| E_Plus			(e, f) -> binary "+" e f
	| E_Minus			(e, f) -> binary "-" e f
	| E_Times			(e, f) -> binary "*" e f
	| E_Divide			(e, f) -> binary "/" e f

	(* Comparison operators *)
	| E_Equal			(e, f) -> binary "="  e f
	| E_NotEqual		(e, f) -> binary "<>" e f
	| E_Greater			(e, f) -> binary ">"  e f
	| E_GreaterEqual	(e, f) -> binary ">=" e f
	| E_Lower			(e, f) -> binary "<"  e f
	| E_LowerEqual		(e, f) -> binary "<=" e f



(* source_statement [statement -> string]
   Returns a re-interpretable representation of a statement *)
let source_statement stmt = begin match stmt with
	| S_Expr (_, e) ->
		source_expr e 0
	| S_LetVal (_, p, e) ->
		sprintf "let %s = %s" (repr_pattern p) (source_expr e 0)
	| S_LetRec (_, n, e) ->
		sprintf "let rec %s = %s" n (source_expr e 0)
	| S_Type (_, name, ctors) ->
		sprintf "type %s = %s\n" name (String.concat " | " ctors)
	end ^ ";;\n"

(* source_program [program -> string]
   Builds a program listing out of the parsed representation *)
let source_program stmts =
	String.concat "\n" (List.map source_statement stmts)
