(*
**	Types - This interpreter's main type definitions
**
**	The interpreter has many mutually-recursive types and sometimes having to
**	define several types plus their interface functions in the same file did
**	not feel like a very good idea, so I gathered the type definitions here and
**	split the functions in other files.
*)

(* range
   Pairs of Lexing.position objects that delimitate expressions in the code. A
   single Lexing.position allows showing the line where the error happens; two
   make it possible to highlight faulty expressions *)
type range = Lexing.position * Lexing.position

(* pattern
   Pattern-matching expressions used by let bindings, match statements and
   indirect bindings in function arguments *)
type pattern =
	| Identifier of string
	| Wildcard
	(* TODO: pattern: Add product types "(x, y)", ADT "Constructor x"... *)

(* Expression types: tree, expr

   This expression type is a simple variant of the ADT that supports
   annotations (additional data attached to every node). I originally
   parameterized the expression type with the annotation type, but eventually
   settled for the record with fixed attributes, for simplicity.

   "expr" is the fully-qualified expression type that the interpreter
   manipulates, and "tree" is the ADT where recursion happens. Making this
   recursion mutual avoids having to put the annotations inside the
   constructors.
*)

type tree =
	(* Literal values and identifiers *)
	| LiteralInt	of int
	| LiteralBool	of bool
	| LiteralUnit
	| Name			of string
	(* Let bindings *)
	| Let			of bool * pattern * expr * expr
	(* Conditionals - if..then without else has unit as else clause *)
	| If			of expr * expr * expr
	(* Functions - curried because fouine has currying *)
	| Function		of pattern * expr
	| Call			of expr * expr
	(* Unary arithmetic operators (int -> int) *)
	| UPlus			of expr
	| UMinus		of expr
	(* Binary arithmetic operators (int -> int -> int) *)
	| Plus			of expr * expr
	| Minus			of expr * expr
	| Times			of expr * expr
	(* Comparison operators (int -> int -> bool) *)
	| Equal			of expr * expr
	| NotEqual		of expr * expr
	| Greater		of expr * expr
	| GreaterEqual	of expr * expr
	| Lower			of expr * expr
	| LowerEqual	of expr * expr

and expr = {
	tree: tree;		(* The expression tree *)
	range: range;	(* Where the expression is located in the source *)
}

(* EnvMap
   A map object that associates strings to, in our case, values *)
module EnvMap = Map.Make(String)

(* StringSet
   Well, a set of strings *)
module StringSet = Set.Make(String)

(* value
   Basically every object that can be found in an environment. Integers and
   booleans are required to run fouine; I added unit so that the if/then
   statement without else could be typed without error. *)
type value =
	| Int of int
	| Bool of bool
	| Unit
	(* Functions retain their binding and body expression node. This type is
	   curried by essence *)
	| Closure of env * pattern * expr

(* env
   As you'd expect - names mapped to values. *)
and env = value EnvMap.t
