open Types
open Util
open Expr
open Eval
open Exceptions

(* Command-line options *)
type options = {
	file:	string;		(* Specifies the file to execute *)
	stdin:	bool;		(* Read from stdin. Has precedence over "file" *)
	ast:	bool;		(* Display the AST before executing *)
	parse:	bool;		(* Stop after parsing (for parsing-only tests) *)
	_ok:	bool;		(* Says whether the configuration is valid *)
}

(* Default configuration mode - run silently *)
let default_conf = {
	file 	= "";
	stdin	= false;
	ast		= false;
	parse	= false;
	_ok		= true;
}

let options_parse argv =
	let rec options_one i conf =
		if i >= Array.length argv then conf else
		match argv.(i) with
		| "-stdin"	-> options_one (i + 1) { conf with stdin = true }
		| "-ast"	-> options_one (i + 1) { conf with ast = true }
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

let _ =
	(* Parse command_line options *)
	let conf = options_parse Sys.argv in
	if not conf._ok then exit 1;

	(* Open the source script *)
	let (channel, name) =
			if conf.stdin then (stdin, "<standard input>")
			else (open_in conf.file, conf.file)
	in

	(* Prepare the lexer and record the file name *)
	let lexbuf = Lexing.from_channel channel in
	lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = name };

	(* Parse the source file *)
	let ast = Parser.toplevel (Lexer.main) lexbuf in

	(* Close the source channel if it's a file *)
	if not conf.stdin then close_in channel;

	(* Display the AST if -ast is on *)
	if conf.ast then expr_print 0 ast;

	(* Stop if -parse is on *)
	if conf.parse then exit 0;

	(* Evaluate the source tree. Exceptions should not reach the toplevel to
	   preserve a return status of 0 if parsing succeeds (automatic testing) *)
	try let _ = eval ast EnvMap.empty in () with
	| TypeError (range, expected, name) ->
		print_string (range_str range ^ ": expected " ^ expected ^ ", got "
		^ name ^ "\n")
	| Failure str -> print_string ("Exception occurred x_x\n'" ^ str ^ "'\n")
