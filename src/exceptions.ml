(*
**	Exceptions - A few kinds of exceptions manipulated by the interpreter
*)

open Types

(* TODO: Try to detect parse errors, and show the location in the terminal *)
exception ParseError

(* MatchError - Occurs when values don't match the patterns or when incomplete
   match statements are exhausted
   @arg		File range of the binding that uses the unmatched pattern
   @arg		The pattern that could not be matched against, None for match
			statements
   @arg		The unexpected value *)
exception MatchError of range * pattern option * value

(* MultiBind - Some variables are bound several times in the same pattern, or
   the same type name is defined twice
   @arg		Whether this is a type definition
   @arg		Range of expression that uses the pattern/type declaration
   @arg		A set of duplicate variables *)
exception MultiBind of bool * range * StringSet.t

(* TypeError - An operator or a statement is used with the wrong kind of value
   @arg		File range corresponding to the faulty expressions
   @arg 	Expected data type
   @arg		Provided data type *)
(* TODO: Use an algebraic type for type names, as required by functions *)
exception TypeError of range * string * string

(* NameError - Occurs when referring to a name that's not defined
   @arg		Source range where the name is mentioned
   @arg		The undefined name *)
exception NameError of range * string

(* InvalidOperator - Occurs when using operators that are not defined in fouine
   and could cause parsing misunderstandings, such as "--", are used
   @arg		Faulty operator name *)
exception InvalidOperator of string

(* ZeroDivision - Exactly what you think it is!
   @arg		Operand that evaluated to zero *)
exception ZeroDivision of range

(* TypeOverload - A single constructor is declared twice
   @arg		Range of the type definition
   @arg		Name of the duplicate constructor *)
exception TypeOverload of range * string

(* UncaughtException - When an exception reaches the toplevel
   @arg		Range of the raise expression
   @arg		The expression object *)
exception UncaughtException of range * value

(* Error - A generic error (fallback)
   @arg		Location of the problem, may be range_empty
   @arg		A textual description of the error *)
exception Error of range * string

(* InternalError - When the interpreter screwed up, and I'm technically
   responsible for it (assertions and pattern matching)
   @arg		A textual description of the problem *)
exception InternalError of string
