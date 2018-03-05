(*
**	Expr - Expression representation and evaluation
*)

(* expr - The tree-like type of expressions *)
type expr =
	(* Literal values and variables *)
	| Expr_LitInt of int
	| Expr_Name of string
	(* Language constructs *)
	| Expr_Let of string * expr * expr
	(* Unary operators *)
	| Expr_Minus of expr
	(* Binary operators *)
	| Expr_Sum of expr * expr
	| Expr_Diff of expr * expr
	| Expr_Prod of expr * expr

(* aexpr - Annotated expressions

   This type is a variant of the previous type, with "annotations": some
   additional data attached to every node. This is helpful to compute types,
   file ranges, propagate errors... the Abstract Syntax Tree uses such a type
   and annotates the nodes with their location in the file.

   It took me some time to come up with this variant. The requirements were:
   - To be able to pattern match like expr just by changing the "match *" part
   - To not have the annotation inside the patterns =
   - To be able to convert with minimal additional syntax *)

type 'a annot_expr =
	(* Literal values *)
	| Aexpr_LitInt of int
	| Aexpr_Name of string
	(* Language constructs *)
	| Aexpr_Let of string * 'a aexpr * 'a aexpr
	(* Unary operators *)
	| Aexpr_Minus of 'a aexpr
	(* Binary operators *)
	| Aexpr_Sum of 'a aexpr * 'a aexpr
	| Aexpr_Diff of 'a aexpr * 'a aexpr
	| Aexpr_Prod of 'a aexpr * 'a aexpr

and 'a aexpr = 'a * 'a annot_expr

(*
	Conversion functions
	The goal here is to provide efficient primitives to avoid overhead when
	manipulating the annotated expressions. The syntax required to turn one
	kind of tree into another should be kept minimal!
*)

(* aexpr_clear - Remove the annotations from a annotated tree *)
let rec aexpr_clear (exp: 'a aexpr) : expr = match snd exp with
	| Aexpr_LitInt(i) -> Expr_LitInt(i)
	| Aexpr_Name(n) -> Expr_Name(n)
	| Aexpr_Let(s, e, f) -> Expr_Let(s, aexpr_clear e, aexpr_clear f)
	| Aexpr_Minus(e) -> Expr_Minus(aexpr_clear e)
	| Aexpr_Sum(e, f) -> Expr_Sum(aexpr_clear e, aexpr_clear f)
	| Aexpr_Diff(e, f) -> Expr_Diff(aexpr_clear e, aexpr_clear f)
	| Aexpr_Prod(e, f) -> Expr_Prod(aexpr_clear e, aexpr_clear f)

(*
	Utility functions and expression manipulation
*)

(* aexpr_print - Print an expression type with annotations
   [('a -> unit) -> int -> 'a aexpr -> unit] *)
let rec aexpr_print annot_str indent exp =

	let recurse (name: string) (children: 'a aexpr list) : unit =
		print_string (name ^ "\n");
		List.iter (aexpr_print annot_str (indent + 1)) children in

	(* Print 2 * 'indent' spaces, the smart way *)
	Printf.printf "(%s) %*s" (Util.range_str (fst exp)) (2 * indent) "";

	match (snd exp) with
	| Aexpr_LitInt i -> print_int i; print_string "\n"
	| Aexpr_Name n -> print_string ("{" ^ n ^ "}\n")
	| Aexpr_Let (name, e, f) -> recurse ("let [" ^ name ^ "] in") [e; f]
	| Aexpr_Minus e -> recurse "-" [e]
	| Aexpr_Sum  (e, f) -> recurse "+" [e; f]
	| Aexpr_Diff (e, f) -> recurse "-" [e; f]
	| Aexpr_Prod (e, f) -> recurse "*" [e; f]
