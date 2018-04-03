(*
**	Types - This interpreter's main type definitions
**
**	The interpreter has many mutually-recursive types. In order to split up
**	interface functions in different modules, I gathered the type definitions
**	in a separated file.
*)

(* range
   Pairs of Lexing.position objects that mark out expressions in the code. A
   single Lexing.position allows showing the line where the error happens; two
   make it possible to highlight faulty expressions. *)
type range = Lexing.position * Lexing.position

(* decl
   Toplevel declarations *)
type decl =
	| D_Expr	of range * expr
	| D_LetVal	of range * pattern * expr
	| D_LetRec	of range * string * expr
	| D_Type	of range * string * string list

(* Expression types: expr_tree, expr

   This expression type is a simple variant of the ADT that supports
   annotations (additional data attached to every node). I originally
   parameterized the expression type with the annotation type, but eventually
   settled for the record with fixed attributes, for simplicity.

   "expr" is the fully-qualified expression type that the interpreter
   manipulates, and "expr_tree" is the ADT where recursion happens. Making this
   recursion mutual avoids having to put the annotations inside the
   constructors.
*)

and expr_tree =
	(* Literal values and identifiers *)
	| E_Int			of int
	| E_Bool		of bool
	| E_Unit
	| E_Name		of string
	(* Type constructors *)
	| E_Ctor		of string * expr
	(* Pattern matching *)
	| Match			of expr * (pattern * expr) list
	(* Let bindings. The semantics of let-value and let-rec are so different
	   that I just split them up. *)
	| E_LetVal		of pattern * expr * expr
	| E_LetRec		of string * expr * expr
	(* Conditionals - if..then without else has a LiteralUnit else clause *)
	| If			of expr * expr * expr
	(* Functions - curried because fouine has currying *)
	| Function		of pattern * expr
	| Call			of expr * expr
	(* References *)
	| Ref			of expr
	| Bang			of expr
	| Assign		of expr * expr
	(* Tuples *)
	| E_Tuple		of expr list
	(* Unary arithmetic operators (int -> int) *)
	| UPlus			of expr
	| UMinus		of expr
	(* Binary arithmetic operators (int -> int -> int) *)
	| Plus			of expr * expr
	| Minus			of expr * expr
	| Times			of expr * expr
	| Divide		of expr * expr
	(* Comparison operators (int -> int -> bool) *)
	| Equal			of expr * expr
	| NotEqual		of expr * expr
	| Greater		of expr * expr
	| GreaterEqual	of expr * expr
	| Lower			of expr * expr
	| LowerEqual	of expr * expr

and expr = {
	tree: expr_tree;	(* The expression tree *)
	range: range;		(* Where the expression is located in the source *)
}

(* pattern
   Pattern-matching expressions used by let bindings, match statements and
   indirect bindings in function arguments *)
and pattern =
	| P_Name of string
	(* Ignored binding "let _ = ..." or wildcard "_" *)
	| P_Wildcard
	(* Literal values, must match exactly. I can't really put a "value" here
	   because patterns are more restricted and can (obviously) be used with
	   types that support comparison; functions don't. *)
	| P_Int of int
	| P_Bool of bool
	| P_Unit
	(* Product types (tuples) *)
	| P_Tuple of pattern list
	(* ADT constructors *)
	| P_Ctor of string * pattern

(* StringMap
   A map object that associates strings to, here, values or type names *)
module StringMap = Map.Make(String)

(* StringSet
   Well, a set of strings *)
module StringSet = Set.Make(String)

(* value
   Basically every object that can be found in an environment. Integers and
   booleans are required to run fouine; I added unit so that the if/then
   statement without else could be typed without error. *)
type value =
	(* Literal values *)
	| V_Int of int
	| V_Bool of bool
	| V_Unit
	(* See "memory.ml" for reference implementation *)
	| V_Ref of memory_addr
	(* Tuples *)
	| V_Tuple of value list
	(* Constructor of an ADT *)
	| V_Ctor of string * value
	(* Functions retain a closure (not a full environment, see below), the name
	   which is recursively bound to the function (or None), their argument and
	   their body. See "eval.ml" for recursion details *)
	| V_Closure of value StringMap.t * string option * pattern * expr
	(* A placeholder captured as a free variable by recursive functions
	   mentioning themselves (this avoids name errors). I could do type
	   inference for recursive functions by inferring the most general type for
	   this placeholder and unifying it with the type of the body *)
	| V_Rec

(* env
   A set of name -> value mappings, as well as a set of name -> type mappings
   for Algebraic Data Types. *)
and env = {
	vars: value StringMap.t;
	types: string StringMap.t;
}

(* memory
   A storage for references. I hoped to make "memory" a type of OCaml-shared
   mutable values. Sharing is not a problem (ADTs like "value" are OK) but the
   mutable requirement only leaves records:
     type memory_element = { mutable data: value; }
   Since this is a reference, and references are not allowed, I'll be using a
   brutal hash table instead. Not an array because of size extensions, but
   that's the idea *)
and memory = (memory_addr, value) Hashtbl.t

(* memory_addr
   THe type of memory addresses. I wanted to keep it opaque *)
and memory_addr = int
