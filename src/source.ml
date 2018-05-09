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
	| E_Ctor (ctor, e)	-> ctor ^ " (" ^ expr level e ^ ")"

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



(* source_machine [machine_program -> string]
   Returns an assembler listing of a machine program *)
let source_machine prog =
	let instruction n inst =
		let str = match inst with
		(* Manipulation of values *)
		| M_Push v		-> sprintf "push    %s" (repr_value v false)
		| M_Tuple n		-> sprintf "tuple   %d" n
		| M_Ctor c		-> sprintf "ctor    \"%s\"" c
		| M_Ref			-> sprintf "ref"
		(* Functional traits *)
		| M_Let p		-> sprintf "let     \"%s\"" (repr_pattern p)
		| M_Match p		-> sprintf "match   \"%s\"" (repr_pattern p)
		| M_EndLet p	-> sprintf "endlet  \"%s\"" (repr_pattern p)
		| M_Access n	-> sprintf "access  \"%s\"" n
		| M_Apply		-> sprintf "apply"
		(* Imperative traits *)
		| M_Jump a		-> sprintf "jump    <%x>" (n + a + 1)
		| M_JumpIf a	-> sprintf "jumpif  <%x>" (n + a + 1)
		| M_Ret			-> sprintf "ret"
		(* Long jumps *)
		| M_Setjmp a	-> sprintf "setjmp  <%x>" (n + a + 1)
		| M_Longjmp		-> sprintf "longjmp"
		(* Operators *)
		| M_Bang		-> sprintf "bang"
		| M_Assign		-> sprintf "assign"
		(* Standard arithmetic *)
		| M_UPlus		-> "plus"
		| M_UMinus		-> "minus"
		| M_Add			-> "add"
		| M_Sub			-> "sub"
		| M_Mul			-> "mul"
		| M_Div			-> "div"
		(* Comparisons between integers *)
		| M_Eq			-> "cmp.eq"
		| M_Ne			-> "cmp.ne"
		| M_Gt			-> "cmp.gt"
		| M_Ge			-> "cmp.ge"
		| M_Lt			-> "cmp.lt"
		| M_Le			-> "cmp.le"
		(* Functions and recursive functions *)
		| M_Close (r, p, a) ->
			begin match r with
			| None		-> sprintf "close   \"%s\" <%x>" (repr_pattern p) a
			| Some n	-> sprintf "close   (\"%s\") \"%s\" <%x>" n
						   (repr_pattern p) a
			end

		in sprintf "%6x:  %s\n" n str in

	let strs = Array.make (Array.length prog) "" in
	for i = 0 to Array.length prog - 1 do
		strs.(i) <- instruction i prog.(i)
	done;
	Array.fold_right (^) strs ""
