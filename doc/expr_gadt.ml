(* Expression types
   Someone suggested me that Generalized Algebraic Data Types were suited for
   this job. They remind me of Coq's inductive types, so I gave them a try.

   The expression type "expr" has a type parameter 't that describes the type
   of the expression. Functions aside, fouine typing is embedded into the GADT
   and type inference is automatically performed by OCaml's type checker.
*)
type _ expr =

	(* Literal values: allowed types are int, bool, unit *)
	| Int		: int -> int expr
	| Bool		: bool -> bool expr
	| Unit		: unit expr

	(* Names could be of any type, we can't know beforehand *)
	| Name		: string -> 't expr

	(* Conditions. Here the GADT enforces the restriction of using only
	   boolean expressions inside the "if" clause. It also requires that
	   both branches have the same type, which is inherited by the
	   statement *)
	| If		: bool expr * 't expr * 't expr -> 't expr


	(* Unary arithmetic operators (int -> int) *)
	| Opp		: int expr -> int expr

	(* Binary arithmetic operators (int -> int -> int) *)
	| Plus		: int expr * int expr -> int expr
	| Minus		: int expr * int expr -> int expr
	| Times		: int expr * int expr -> int expr

	(* Comparison operators (int -> int -> bool)
	   Let's use polymorphic comparison (just for fun) *)
	| Equal		: 't expr * 't expr -> bool expr
	| NotEqual	: 't expr * 't expr -> bool expr
	| Greater	: 't expr * 't expr -> bool expr
	| GreaterEqual	: 't expr * 't expr -> bool expr
	| Lower		: 't expr * 't expr -> bool expr
	| LowerEqual	: 't expr * 't expr -> bool expr

(* See it in action:
	# Name "x";;
	- : 'a expr = Name "x"
	# Plus (Name "x", Int 2);;
	- : int expr = Plus (Name "x", Int 2)
	# Equal (Name "x", Name "y");;
	- : bool expr = Equal (Name "x", Name "y")
	# If (Bool true, Name "x", Name "y");;
	- : 'a expr = If (Bool true, Name "x", Name "y")
	# If (Int 2, Name "x", Name "y");;
	Error: This expression has type int expr
	       but an expression was expected of type bool expr
	       Type int is not compatible with type bool
*)
