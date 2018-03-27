open Types
open Util
open Expr
open Eval
open Exceptions
open Pattern
open Lexing

(* Command-line options *)
type options = {
	file:	string;		(* Specifies the file to execute *)
	stdin:	bool;		(* Read from stdin. Has precedence over "file" *)
	ast:	bool;		(* Display the AST before executing *)
	debug:	bool;		(* Show program source, akin to -ast *)
	parse:	bool;		(* Stop after parsing (for parsing-only tests) *)
	_ok:	bool;		(* Says whether the configuration is valid *)
}

(* Default configuration mode - run silently *)
let default_conf = {
	file 	= "";
	stdin	= false;
	ast		= false;
	debug	= false;
	parse	= false;
	_ok		= true;
}

(* options_parse [string array -> options]
   Parses command-line arguments. Emits live warnings if the options are not
   consistent, and clears the _ok bit of the returned record if critical errors
   are encountered *)
let options_parse argv =
	let rec options_one i conf =
		if i >= Array.length argv then conf else
		match argv.(i) with
		| "-stdin"	-> options_one (i + 1) { conf with stdin = true }
		| "-ast"	-> options_one (i + 1) { conf with ast = true }
		| "-debug"	-> options_one (i + 1) { conf with debug = true }
		| "-parse"	-> options_one (i + 1) { conf with parse = true }
		| filename	-> options_one (i + 1) { conf with file = filename } in

	(* Perform some checks *)
	let conf = ref (options_one 1 default_conf) in

	(* Do not specify both a file name and stdin input *)
	if !conf.file <> "" && !conf.stdin then begin
		conf := { !conf with file = "" };
		print_string ("warning: -stdin overrides file '" ^ !conf.file ^ "'\n")
	end;

	(* Specify at least one of file input and stdin input *)
	if !conf.file = "" && not !conf.stdin then begin
		conf := { !conf with _ok = false };
		print_string "error: no file name and -stdin not specified\n"
	end;

	!conf

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

let s_get_this_show_on_the_road =
	(* Parse command_line options *)
	let conf = options_parse Sys.argv in
	if not conf._ok then exit 1;

	(* Open the source script *)
	let (channel, name) =
			if conf.stdin then (stdin, "")
			else (open_in conf.file, conf.file)
	in

	(* Prepare the lexer and record the file name *)
	let lexbuf = Lexing.from_channel channel in
	lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };

	(* Parse the source file *)
	let ast = Parser.toplevel (Lexer.main) lexbuf in

	(* Close the source channel if it's a file *)
	if not conf.stdin then close_in channel;

	(* Display the source if -debug is on, or the AST if -ast is on *)
	if conf.debug then expr_source 0 ast
	else if conf.ast then expr_print 0 ast;

	(* Stop now if -parse is on, we don't need to evaluate *)
	if conf.parse then exit 0;

	(* Evaluate the source tree. Exceptions are caught here (nowhere else) *)
	let failed = exception_report (fun () ->
		let env = { vars = StringMap.empty; types = StringMap.empty } in
		eval ast env
	) in
	exit (if failed then 1 else 0)
