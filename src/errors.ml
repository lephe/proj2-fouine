(*
**	Exceptions - A few kinds of exceptions manipulated by the interpreter
*)

open Types
open Repr
open Range
open Printf
open Exceptions

(* errors_try [(unit -> 'a) -> bool]
   Executes the provided function, catching possible errors and reporting them
   on stderr. Returns true if an exception occurred, false otherwise *)
let errors_try func =
	let pre range =
		let r = repr_range range in
		fprintf stderr "\x1b[1m%s: \x1b[31merror: \x1b[0m" r in
	let ret = ref true in begin

	try let _ = func () in ret := false with
	| MatchError (r, Some pat, value) -> pre r;
		fprintf stderr "cannot match value %s against '%s'\n"
			(repr_value value false) (repr_pattern pat);
		range_highlight r stderr
	| MatchError (r, None, value) -> pre r;
		fprintf stderr "cannot match value %s in this statement\n"
			(repr_value value false);
		range_highlight r stderr
	| MultiBind (false, r, set) -> pre r;
		fprintf stderr ("some variables are bound twice in this " ^^
			"pattern:\n ");
		StringSet.iter (fun s -> fprintf stderr " %s" s) set;
		fprintf stderr "\n";
		range_highlight r stderr
	| TypeError (r, exp, name) -> pre r;
		fprintf stderr "expected %s, got %s\n" exp name;
		range_highlight r stderr
	| NameError (r, name) -> pre r;
		fprintf stderr "undefined name '%s'\n" name;
		range_highlight r stderr
	| InvalidOperator name ->
		Printf.printf "\x1b[31merror: \x1b[0m invalid operator '%s'\n" name
	| ZeroDivision r -> pre r;
		fprintf stderr "division by zero\n";
		range_highlight r stderr
	| TypeOverload (r, name) -> pre r;
		fprintf stderr "constructor '%s' is declared twice\n" name;
		range_highlight r stderr
	| Failure str
	| InternalError str ->
		fprintf stderr "error: an exception occurred x_x\n'%s'\n" str

	end; !ret

