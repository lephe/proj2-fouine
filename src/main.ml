open Types
open Util
open Expr
open Eval
open Exceptions
open Pattern
open Lexing
open Shell
open Toplevel

(* Command-line options *)
type options = {
	file:	string;		(* Specifies the file to execute *)
	stdin:	bool;		(* Read from stdin (has precedence over <file> *)
	shell:	bool;		(* Use the interactive REPL shell (fallback) *)

	ast:	bool;		(* Display the AST before executing *)
	debug:	bool;		(* Show program source, akin to -ast *)
	parse:	bool;		(* Stop after parsing (for parsing-only tests) *)

	help:	bool;		(* Show help message and return *)
	_ok:	bool;		(* Says whether the configuration is valid *)
}

(* Default configuration mode - run silently *)
let default_conf = {
	file 	= "";
	stdin	= false;
	shell	= false;

	ast		= false;
	debug	= false;
	parse	= false;

	help	= false;
	_ok		= true;
}

(* options_parse [string array -> options]
   Parses command-line arguments. Emits live warnings if the options are not
   consistent, and clears the _ok bit of the returned record if critical errors
   are encountered *)
let options_parse argv =
	let rec options_one i conf =
		if i >= Array.length argv then conf else
		let conf' = match argv.(i) with
		| "--help"	-> { conf with help = true }

		| "-stdin"	-> { conf with stdin = true }

		| "-ast"	-> { conf with ast = true }
		| "-debug"	-> { conf with debug = true }
		| "-parse"	-> { conf with parse = true }
		| filename	-> { conf with file = filename } in
		options_one (i + 1) conf' in

	(* Perform some checks *)
	let conf = ref (options_one 1 default_conf) in

	if !conf.help then !conf else begin

	(* Do not specify both a file name and stdin input *)
	if !conf.file <> "" && !conf.stdin then begin
		conf := { !conf with file = "" };
		print_string ("warning: -stdin overrides file '" ^ !conf.file ^ "'\n")
	end;

	(* Specify at least one of file input and stdin input *)
	if !conf.file = "" && not !conf.stdin then
		conf := { !conf with shell = true };

	(* Help message in case something goes wrong *)
	if not (!conf._ok) then
		Printf.fprintf stderr "See '%s --help' for command details.\n"argv.(0);

	!conf end

(* exception_report [(unit -> unit) -> bool]
   A wrapper for exception reporting in the terminal. Returns true if an
   exception occurred, false otherwise *)
let exception_report func =
	let pre range =
		let r = range_str range in
		Printf.fprintf stderr "\x1b[1m%s: \x1b[31merror: \x1b[0m" r in
	let ret = ref true in begin

	try func (); ret := false with
	| MatchError (r, Some pat, value) -> pre r;
		Printf.fprintf stderr "cannot match value %s against '%s'\n"
			(value_str value) (pattern_str pat);
		range_highlight r stderr
	| MatchError (r, None, value) -> pre r;
		Printf.fprintf stderr "cannot match value %s in this statement\n"
			(value_str value);
		range_highlight r stderr
	| MultiBind (false, r, set) -> pre r;
		Printf.fprintf stderr ("some variables are bound twice in this " ^^
			"pattern:\n ");
		StringSet.iter (fun s -> Printf.fprintf stderr " %s" s) set;
		Printf.fprintf stderr "\n";
		range_highlight r stderr
	| TypeError (r, exp, name) -> pre r;
		Printf.fprintf stderr "expected %s, got %s\n" exp name;
		range_highlight r stderr
	| NameError (r, name) -> pre r;
		Printf.fprintf stderr "undefined name '%s'\n" name;
		range_highlight r stderr
	| InvalidOperator name ->
		Printf.printf "\x1b[31merror: \x1b[0m invalid operator '%s'\n" name
	| ZeroDivision r -> pre r;
		Printf.fprintf stderr "division by zero\n";
		range_highlight r stderr
	| TypeOverload (r, name) -> pre r;
		Printf.fprintf stderr "constructor '%s' is declared twice\n" name;
		range_highlight r stderr
	| Failure str
	| InternalError str ->
		Printf.fprintf stderr "error: an exception occurred x_x\n'%s'\n" str

	end; !ret

(* The help message for the command-line interface *)
let help_message =
"fouine - an interpreter for a subset of Caml\n" ^
"usage: fouine [options...] <file>\n" ^
"       fouine [-stdin] [options...]\n" ^
"\n" ^
"Input options:\n" ^
"  <file>    Execute <file>, then leave\n" ^
"  -stdin    Read from stdin (expect shorter error diagnoses)\n" ^
"  (none)    Start the Read-Eval-Print Loop shell\n" ^
"\n" ^
"Debugging options:\n" ^
"  -ast      After parsing, show an AST\n" ^
"  -debug    After parsing, show regenerated sources (overrides -ast)\n" ^
"  -parse    Stop after parsing; do not evaluate\n"

let s_get_this_show_on_the_road =
	(* Parse command_line options *)
	let conf = options_parse Sys.argv in
	if not conf._ok then exit 1;

	(* Show help message if requested - then leave *)
	if conf.help then (print_string help_message; exit 0);

	(* Start the REPL mode when -repl is specified *)
	if conf.shell then (shell_main conf; exit 0);

	(* Open the source script *)
	let (channel, name) =
			if conf.stdin then (stdin, "")
			else (open_in conf.file, conf.file)
	in

	(* Prepare the lexer and record the file name *)
	let lexbuf = Lexing.from_channel channel in
	lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };

	(* Parse the source file *)
	let decls = Parser.toplevel (Lexer.main) lexbuf in

	(* Close the source channel if it's a file *)
	if not conf.stdin then close_in channel;

	(* Display the source if -debug is on, or the AST if -ast is on *)
	if conf.debug then List.iter decl_source decls
	else if conf.ast then List.iter decl_print decls;

	(* Stop now if -parse is on, we don't need to evaluate *)
	if conf.parse then exit 0;

	(* Evaluate the source tree. Exceptions are caught here (nowhere else) *)
	let env_vars = StringMap.empty in
	(* I define the Empty and Cons constructors by default, for lists *)
	let defaults = [ ("Empty", "list"); ("Cons", "list") ] in
	let env_types = List.fold_left (fun m (x, y) -> StringMap.add x y m)
		StringMap.empty defaults in

	let failed = exception_report (fun () ->
		let env = { vars = env_vars; types = env_types } in
		let eval_one env decl = fst (decl_eval decl env) in
		List.fold_left eval_one env decls
	) in
	exit (if failed then 1 else 0)
