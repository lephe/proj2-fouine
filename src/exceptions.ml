(*
**	Exceptions - A few kinds of exceptions manipulated by the interpreter
*)

open Types

(* TODO: Add a type for syntax errors and try to highlight the corresponding
   location in the file *)

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

(* InternalError - When the interpreter screwed up, and I'm technically
   responsible for it (assertions and pattern matching)
   @arg [string]	A textual description of the problem *)
exception InternalError of string
