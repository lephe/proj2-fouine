(*
**	Main - Command-line parsing and main flow for non-interactinve invocations
*)

open Types
open Errors

open Interpreter
open Shell

open Repr
open Source

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

(* config_parse [string array -> config]
   Parses command-line arguments. Emits live warnings if the options are not
   consistent, and clears the _ok bit of the returned record if critical errors
   are encountered *)
let config_parse argv =
	let rec config_one i conf =
		if i >= Array.length argv then conf else
		let conf' = match argv.(i) with
		| "--help"	-> { conf with help = true }

		| "-stdin"	-> { conf with stdin = true }

		| "-ast"	-> { conf with ast = true }
		| "-debug"	-> { conf with debug = true }
		| "-parse"	-> { conf with parse = true }
		| filename	-> { conf with file = filename } in
		config_one (i + 1) conf' in

	(* Perform some checks *)
	let conf = ref (config_one 1 default_conf) in

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

	!conf; end



let s_get_this_show_on_the_road =

	(* Parse command_line options *)
	let conf = config_parse Sys.argv in
	if not conf._ok then exit 1;

	(* Show the help message if --help is specified (then leave) *)
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
	let program = Parser.toplevel (Lexer.main) lexbuf in

	(* Close the source channel if it's a file *)
	if not conf.stdin then close_in channel;

	(* Display the source if -debug is on, or the AST if -ast is on *)
	if conf.debug then print_string (source_program program)
	else if conf.ast then print_string (repr_program program);

	(* Stop now if -parse is on, we don't need to evaluate *)
	if conf.parse then exit 0;

	(* Create a fresh execution environment *)
	let env = interpreter_start () in
	(* This function evaluates a single statement *)
	let eval_one env stmt = fst (interpreter_exec stmt env) in

	(* Exceptions are caught here! *)
	let final_env = errors_try (fun () -> List.fold_left eval_one env program)
	in exit (if final_env = None then 1 else 0)
