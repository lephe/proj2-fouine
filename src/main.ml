(*
**	Main - Command-line parsing and main flow for non-interactinve invocations
*)

open Types
open Errors
open Lexing

open Transform
open Interpreter
open Shell
open Machine

open Repr
open Source

(* The help message for the command-line interface *)
let help_message =
"fouine - an interpreter for a subset of Caml\n" ^
"usage: fouine [options...] <file>\n" ^
"       fouine [-stdin] [options...]\n" ^
"\n" ^
"Input options:\n" ^
"  <file>      Execute <file>, then leave\n" ^
"  -stdin      Read from stdin (expect shorter error diagnoses)\n" ^
"  (none)      Start the Read-Eval-Print Loop shell\n" ^
"\n" ^
"Experimental options:\n" ^
"  -typing     Enable type inference (enabled by default in shell)\n" ^
"  -no-typing  Disable type inference\n" ^
"\n" ^
"Debugging options:\n" ^
"  -ast        After parsing, show an AST\n" ^
"  -debug      After parsing, show regenerated sources (overrides -ast)\n" ^
"  -parse      Stop after parsing; do not evaluate\n" ^
"\n" ^
"Transformation options:\n" ^
"  -E          Eliminate exceptions using continuations\n" ^
"  -R          Eliminate imperative traits using memory-passing\n" ^
"  -ER, -RE    Eliminate both (last first, first last)\n" ^
"  -outcode    Print resulting code after transformation\n" ^
"\n" ^
"Stack machine options:\n" ^
"  -machine    Compile to a stack machine before executing\n" ^
"  -stackcode  Print the machine code (only execute is -machine is on)\n"

(* Default configuration mode - run silently *)
let default_conf = {
	file 		= "";
	stdin		= false;
	shell		= false;

	typing		= None;

	ast			= false;
	debug		= false;
	parse		= false;

	transf		= [];
	outcode		= false;
	machine		= false;
	stackcode	= false;

	help		= false;
	_ok			= true;
}

(* config_parse [string array -> config]
   Parses command-line arguments. Emits live warnings if the options are not
   consistent, and clears the _ok bit of the returned record if critical errors
   are encountered *)
let config_parse argv =
	let rec config_one i conf =
		if i >= Array.length argv then conf else
		let conf' = match argv.(i) with

		| "--help"		-> { conf with help = true }
		| "-stdin"		-> { conf with stdin = true }

		| "-typing"		-> { conf with typing = Some true }
		| "-no-typing"	-> { conf with typing = Some false }

		| "-R"			-> { conf with transf = [ 'R' ] }
		| "-E"			-> { conf with transf = [ 'E' ] }
		| "-ER"			-> { conf with transf = [ 'R'; 'E' ] }
		| "-RE"			-> { conf with transf = [ 'E'; 'R' ] }
		| "-outcode"	-> { conf with outcode = true }

		| "-machine"	-> { conf with machine = true }
		| "-stackcode"	-> { conf with stackcode = true }

		| "-ast"		-> { conf with ast = true }
		| "-debug"		-> { conf with debug = true }
		| "-parse"		-> { conf with parse = true }
		| filename		-> { conf with file = filename }

		in config_one (i + 1) conf' in

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
	let (channel, name) = if conf.stdin
	then (stdin, "")
	else (open_in conf.file, conf.file) in

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

	(* Transform the program if any of -E, -R, -ER or -RE is specified *)
	let program = transform program conf.transf in

	(* If -machine or -stackcode is specified, enter the stack machine mode *)
	if conf.machine || conf.stackcode then begin
		let code = machine_compile program conf.transf in
		(* Print the source if -stackcode is specified *)
		if conf.stackcode then print_string (source_machine code);
		if conf.stackcode && conf.machine then print_newline ();
		(* Execute the program if -machine is specified *)
		let result = if conf.machine
		then errors_try (fun () -> machine_exec code conf.transf)
		else Some () in
		(* Leave now since we don't interpret the Fouine program as is *)
		exit (if result = None then 1 else 0)
	end;

	(* Print the resulting source if -outcode is on *)
	if conf.transf <> [] && conf.outcode
	then print_string (source_program program);

	(* Stop now if -parse is on, we don't need to evaluate *)
	if conf.parse then exit 0;

	(* In execution mode, disable typing by default *)
	let typing = match conf.typing with Some b -> b | None -> false in
	(* Create a fresh execution environment *)
	let env = interpreter_start conf.transf in
	(* This function evaluates a single statement *)
	let eval_one env stmt = fst (interpreter_exec typing stmt env) in

	(* Exceptions are caught here! *)
	let final_env = errors_try (fun () -> List.fold_left eval_one env program)
	in exit (if final_env = None then 1 else 0)
