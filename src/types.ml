(*
**	Types - This interpreter's main type definitions
**
**	The interpreter has many mutually-recursive types. In order to split up
**	interface functions in different modules, I gathered the type definitions
**	in a separated file.
*)

(*
**	Functor-generated modules (sets and maps)
*)

module StringSet = Set.Make(String)
module StringMap = Map.Make(String)

module IntSet = Set.Make(struct
	type t = int
	let compare = Pervasives.compare
end)

module IntMap = Map.Make(struct
	type t = int
	let compare = Pervasives.compare
end)



(*
**	Command-line options
*)

type config = {
	file:		string;		(* Specifies the file to execute *)
	stdin:		bool;		(* Read from stdin (has precedence over <file> *)
	shell:		bool;		(* Use the interactive REPL shell (fallback) *)

	ast:		bool;		(* Display the AST before executing *)
	debug:		bool;		(* Show program source, akin to -ast *)
	parse:		bool;		(* Stop after parsing (for parsing-only tests) *)

	transf:		char list;	(* Transformations to apply (ordered) *)
	outcode:	bool;		(* Print transformed code *)
	machine:	bool;		(* Use the stack machine to execute the program *)
	stackcode:	bool;		(* Print the compiled code *)

	help:		bool;		(* Show help message and return *)
	_ok:		bool;		(* Says whether the configuration is valid *)
}



(*
**	Typing-related definitions
*)

(* Type variables, represented here by integers *)
type typevar = int

(* Monotypes with type variables (called "types" in Damas & Milner's paper *)
type mtype =
	(* Type variables *)
	| T_Var			of typevar
	(* Usual base types *)
	| T_Int
	| T_Bool
	| T_Unit
	(* Type constructors *)
	| T_Fun			of mtype * mtype
	| T_Product		of int * mtype list
	| T_List		of mtype
	(* Named ADTs *)
	| T_ADT			of string

(* Polytypes, aka type schemes in the original paper *)
type ptype = IntSet.t * mtype



(*
**	Interpreter-related data types
*)

(* range
   Pairs of Lexing.position objects that mark out expressions in the code. A
   single Lexing.position allows showing the line where the error happens; two
   make it possible to highlight code samples *)
type range = Lexing.position * Lexing.position

(* pattern
   Pattern-matching expressions used by let bindings, match statements and
   indirect bindings in function arguments *)
type pattern =
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

type expr_tree =
	(* Literal values and identifiers *)
	| E_Int				of int
	| E_Bool			of bool
	| E_Unit
	| E_Name			of string
	(* Type constructors *)
	| E_Ctor			of string * expr
	(* Pattern matching and try .. with statements *)
	| E_Match			of expr * (pattern * expr) list
	| E_Try				of expr * (pattern * expr) list
	(* Let bindings. The semantics of let-value and let-rec are so different
	   that I just split them up. *)
	| E_LetVal			of pattern * expr * expr
	| E_LetRec			of string * expr * expr
	(* Conditionals - if..then without else has a E_Unit else clause *)
	| E_If				of expr * expr * expr
	(* Functions - curried because fouine has currying *)
	| E_Function		of pattern * expr
	| E_Call			of expr * expr
	(* References *)
	| E_Ref				of expr
	| E_Bang			of expr
	| E_Assign			of expr * expr
	(* Tuples *)
	| E_Tuple			of expr list
	(* Unary arithmetic operators (int -> int) *)
	| E_UPlus			of expr
	| E_UMinus			of expr
	(* Binary arithmetic operators (int -> int -> int) *)
	| E_Plus			of expr * expr
	| E_Minus			of expr * expr
	| E_Times			of expr * expr
	| E_Divide			of expr * expr
	(* Comparison operators (int -> int -> bool) *)
	| E_Equal			of expr * expr
	| E_NotEqual		of expr * expr
	| E_Greater			of expr * expr
	| E_GreaterEqual	of expr * expr
	| E_Lower			of expr * expr
	| E_LowerEqual		of expr * expr

and expr = {
	tree: expr_tree;	(* The expression tree *)
	range: range;		(* Where the expression is located in the source *)
}

(* A shortcut to make an expression out of a range and a tree. Often used as
   "r% tree" to hide the boring record truth *)
let (%) range tree = { range = range; tree = tree }

(* statement
   Top-level instructions at the root of the source tree (sequential) *)
type statement =
	| S_Expr	of range * expr
	| S_LetVal	of range * pattern * expr
	| S_LetRec	of range * string * expr
	| S_Type	of range * string * string list

(* program
   Just put up statements together and you get a full program *)
type program = statement list

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
	(* Built-in functions that rely on an OCaml implementation *)
	| V_Builtin of (range -> value -> env -> (value -> value) -> value)
	(* A memory - used only by programs that went under -R, -RE or -ER *)
	| V_Memory of int * memory
	(* Machine closures used by the stack machine *)
	| V_MachineClosure of pattern * machine_env * int
	(* Machine stack frame *)
	| V_MachineFrame of machine_env * int
	(* Machine builtings *)
	| V_MachineBuiltin of (machine_program -> int -> machine_stack ->
		machine_env -> machine_state list -> value -> unit)

(* exch
   Exception handler for try statements. An single expression for each handled
   exception type, but we also need to capture the environment in which the try
   statement is executed, otherwise we won't unwind the stack at all! We also
   save the continuation to be able to resume execution *)
and exch = env * StringSet.t * (value -> value) * (pattern * expr) list

(* env
   A set of name -> value mappings, as well as a set of name -> type mappings
   for Algebraic Data Types. *)
and env = {
	vars:  value StringMap.t;
	types: string StringMap.t;
	exchs: exch list;
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
   The type of memory addresses. I wanted to keep it opaque *)
and memory_addr = int



(*
**	Shell-specific types
*)

(* event
   Noticeable events that happen during program execution. These are emitted by
   the Interpreter module and typically reported by the interactive shell *)
(* TODO: Add type definitions events! *)
(* TODO: Add "exit function was called" event *)
and event =
	| Ev_Result of value
	| Ev_Binding of string * value



(*
**	Compilation-related types
*)

and machine_stack =
	value list

and machine_instr =
	(* Manipulation of values *)
	| M_Push		of value			(* Push constant to stack *)
	| M_Tuple		of int				(* Make a tuple *)
	| M_Ctor		of string			(* Instantiate an ADT *)
	| M_Ref								(* Create a reference *)
	(* Functional traits *)
	| M_Let			of pattern			(* Move a value to the environment *)
	| M_Match		of pattern			(* Try to bind, return match status *)
	| M_EndLet		of pattern			(* Pop values from the environment *)
	| M_Access		of string			(* Access variable *)
	| M_Close		of string option * pattern * int
	| M_Apply							(* Apply function *)
	(* Imperative traits *)
	| M_Jump		of int				(* Jump to offset *)
	| M_JumpIf		of int				(* Jump to offset if condition holds *)
	| M_Ret								(* Return from function *)
	(* Long jumps for try .. with statements *)
	| M_Setjmp		of int				(* Save machine state *)
	| M_Longjmp							(* Restore machine state *)
	(* Operators *)
	| M_Bang | M_Assign
	(* Standard arithmetic *)
	| M_UPlus | M_UMinus | M_Add | M_Sub | M_Mul | M_Div
	(* Comparisons between integers *)
	| M_Eq | M_Ne | M_Gt | M_Ge | M_Lt | M_Le

and machine_program =
	machine_instr array

and machine_env =
	(string * value) list

and machine_state =
	int * machine_stack * machine_env
