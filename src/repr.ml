(*
**	Repr - String representations of objects
*)

open Types
open Lexing
open Memory
open Printf

(* repr_memory_addr
   Human-readable form (this is just an integer anyway) *)
let repr_memory_addr = string_of_int

(* repr_range [range -> string]
   Returns a string on the form "file:line:column" that pinpoints the starting
   position of the range *)
let repr_range (s, e) : string =
	let cnum = s.pos_cnum - s.pos_bol + 1 in
	let name = if s.pos_fname = "" then "<standard input>" else s.pos_fname in
	sprintf "%s:%02d:%02d" name s.pos_lnum cnum

(* repr_pattern [pattern -> string]
   Returns a human and OCaml-readable representation of a pattern *)
let rec repr_pattern = function
	| P_Name i		-> i
	| P_Wildcard	-> "_"
	| P_Int i		-> string_of_int i
	| P_Bool b		-> if b then "true" else "false"
	| P_Unit		-> "()"
	| P_Tuple l		-> "(" ^ String.concat ", " (List.map repr_pattern l) ^ ")"
	| P_Ctor (c, p)	-> c ^ " " ^ repr_pattern p



(* repr_expr [expr -> int -> string]
   Produces the syntax tree of an expression, indenting each line with the
   requested amount of spaces. Has a final newline *)
let rec repr_expr exp indent =

	(* space [string]
	   File range header and requested indent *)
	let space =
		sprintf "(%s) %*s" (repr_range exp.range) indent "" in

	(* recurse_n [string -> expr list -> int -> string]
       Returns "str" for the current level, then recurses on "children" after
	   adding "diff" to the indent level *)
	let recurse_n (str: string) (children: expr list) diff =
		let txt = List.map (fun e -> repr_expr e (indent + diff)) children in
		str ^ "\n" ^ (String.concat "" txt) in

	(* recurse [string -> expr list -> string]
	   Most common recursion scheme with diff 2 (a single level) *)
	let recurse str children =
		recurse_n str children 2 in

	space ^ match exp.tree with

	(* Atomic elements *)

	| E_Int i	-> sprintf "int<%d>\n" i
	| E_Bool b	-> if b then "bool<true>\n" else "bool<false>\n"
	| E_Unit	-> "unit\n"
	| E_Name n	-> sprintf "name<%s>\n" n
	| E_Ctor (ctor, e) -> recurse (sprintf "constructor<%s>" ctor) [e]

	(* Language constructs. For "match" statements, I indent more because I
	   want to have "match", the patterns and the associated expressions on 3
	   different indentation levels *)

	| E_Match (e, cl) ->
		recurse "match" [e] ^
		String.concat "" (List.map (fun (p, e) ->
			space ^ "  " ^ recurse_n (repr_pattern p ^ " -> ") [e] 4
		) cl)
	| E_Try (e, cl) ->
		recurse "try" [e] ^
		String.concat "" (List.map (fun (p, e) ->
			space ^ "  " ^ recurse_n (repr_pattern p ^ " -> ") [e] 4
		) cl)

	| E_LetVal (pat, e, f) ->
		recurse ("let_in " ^ repr_pattern pat) [e; f]
	| E_LetRec (name, e, f) ->
		recurse ("let_rec " ^ name) [e; f]

	| E_If (e, t, f) ->
		if f.tree = E_Unit then recurse "if_then" [e; t]
		else recurse "if_then_else" [e; t; f]

	| E_Function (pat, e) ->
		recurse ("fun<" ^ repr_pattern pat ^ ">") [e]
	| E_Call (f, v) ->
		recurse "call" [f; v]

	(* Recursion on operators *)

	| E_Bang e			-> recurse "!" [e]
	| E_Ref e 			-> recurse "ref" [e]
	| E_Assign (e, f)	-> recurse ":=" [e; f]

	| E_Tuple l			-> recurse "tuple" l

	| E_UPlus  e		-> recurse "+/1" [e]
	| E_UMinus e		-> recurse "-/1" [e]

	| E_Plus			(e, f) -> recurse "+" [e; f]
	| E_Minus			(e, f) -> recurse "-" [e; f]
	| E_Times			(e, f) -> recurse "*" [e; f]
	| E_Divide			(e, f) -> recurse "/" [e; f]

	| E_Equal			(e, f) -> recurse "="  [e; f]
	| E_NotEqual		(e, f) -> recurse "<>" [e; f]
	| E_Greater			(e, f) -> recurse ">"  [e; f]
	| E_GreaterEqual	(e, f) -> recurse ">=" [e; f]
	| E_Lower			(e, f) -> recurse "<"  [e; f]
	| E_LowerEqual		(e, f) -> recurse "<=" [e; f]



(* repr_value [value -> string]
   Builds a printable (not unambiguous) representation of a value *)
let rec repr_value value verbose = match value with

	(* Non-recursive values *)
	| V_Int i		-> string_of_int i
	| V_Bool b		-> if b then "true" else "false"
	| V_Unit		-> "()"

	(* TODO: repr_value: Use a better  syntax for lists *)
	| V_Ctor ("Cons", V_Tuple [a; b]) ->
		repr_value a false ^ " :: " ^ repr_value b false
	| V_Ctor ("Empty", _) -> "[]"
	| V_Ctor (c, e)	-> c ^ " " ^ repr_value e false

	(* Recursive constructs *)
	| V_Ref r -> "ref " ^ repr_value (memory_get r) false
	| V_Tuple l ->
		let recurse v = repr_value v false in
		"(" ^ String.concat ", " (List.map recurse l) ^ ")"

	(* Closures can be fully displayed *)
	| V_Closure (closure, recursive, pat, e) ->
		if not verbose then "<fun>" else

		let head = match recursive with
		| None -> "<closure>"
		| Some n -> sprintf "<recursive closure '%s'>" n in

		let make_one name value start =
			sprintf "%s\n  %s = %s" start name (repr_value value false) in
		StringMap.fold make_one closure head

	(* Built-in functions, memory objects *)
	| V_Builtin f -> "<builtin>"
	| V_Memory (_, m) -> "<builtin memory>"

	(* Stack machine-related values *)
	| V_MachineClosure (_, env, addr) ->
		let head = sprintf "<closure %x>" addr in
		if not verbose then head else

		let make_one start (name, value) =
			sprintf "%s\n  %s = %s" start name (repr_value value false) in
		List.fold_left make_one head env
	| V_MachineFrame (_, addr) -> sprintf "<frame %x>" addr
	| V_MachineBuiltin f -> "<builtin>"



(* repr_statement [statement -> string]
   Produces a syntax tree for a statement, very much like repr_expr *)
let repr_statement stmt = match stmt with
	| S_Expr (_, e) ->
		repr_expr e 0
	| S_LetVal (r, p, e) ->
		sprintf "(%s) let %s =\n%s"
		(repr_range r) (repr_pattern p) (repr_expr e 2)
	| S_LetRec (r, n, e) ->
		sprintf "(%s) let rec %s =\n%s"
		(repr_range r) (n) (repr_expr e 2)
	| S_Type (r, name, ctors) ->
		let elements = String.concat "\n" (List.map (fun c ->
			sprintf "(%s)   %s" (repr_range r) c
		) ctors) in
		sprintf "(%s) type %s\n%s" (repr_range r) name elements

(* repr_program [program -> string]
   Returns a printable version of a statement list *)
let repr_program stmts =
	String.concat "" (List.map repr_statement stmts)
