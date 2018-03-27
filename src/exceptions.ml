(*
**	Exceptions - A few kinds of exceptions manipulated by the interpreter
*)

open Types

(* TODO: Try to detect parse errors, and show the location in the terminal *)
exception ParseError

(* MatchError - Occurs when patterns don't match value, when a recursive value
   is requested, or when patterns and arguments are mixed...
   @arg [range]		File range of the let-binding that uses the invalid pattern
   @arg [pattern]	The pattern that could not be matched against
   @arg [value]		The unexpected value *)
exception MatchError of range * pattern * value

(* MultiBind - Some variables are bound several times in the same pattern, or
   the same type name is defined twice
   @arg [bool]		Whether this is a type definition
   @arg [range]		Range of expression that uses the pattern/type declaration
   @arg [StringSet.t] List of duplicate variables *)
exception MultiBind of bool * range * StringSet.t

(* TypeError - Occurs during evaluation if an operator or a statement is used
   with the wrong kind of object
   @arg [range]		File range corresponding to the faulty expressions
   @arg [string] 	Expected data type
   @arg [string]	Provided data type *)
(* TODO: Update this description once static typing is up
   TODO: Use an algebraic type for type names, as required by functions *)
exception TypeError of range * string * string

(* NameError - Occurs when referring to a name that's not defined
   @arg [range]		File range where the name is mentioned
   @arg [string]	The undefined name *)
exception NameError of range * string

(* InvalidOperator - Occurs when using operators that are not defined in fouine
   and could cause parsing misunderstandings, such as "--"
   @arg [string]	Faulty operator name *)
exception InvalidOperator of string

(* ZeroDivision - Exactly what you think it is!
   @arg [range]		Operand that evaluated to zero *)
exception ZeroDivision of range

(* TypeOverload - A single constructor is declared twice *)
exception TypeOverload of range * string

(* Error - A generic error (fallback)
   @arg [range]		Location of the problem, may be range_empty
   @arg [string]	A textual description of the error *)
exception Error of range * string

(* InternalError - When the interpreter screwed up, and I'm technically
   responsible for it (assertions and pattern matching)
   @arg [string]	A textual description of the problem *)
exception InternalError of string
