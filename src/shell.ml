(*
**	Shell - Interactive Read-Eval-Print Loop shell
*)

open Types
open Interpreter
open Repr
open Printf

(* Number of the currently-executed shell command [private] *)
let shell_num = ref 0

(* get_command [unit -> decl list] [private]
   Reads and parses a command from standard input *)
let get_command () =
	(* Start the lexer with the command number as file name *)
	let input_name = "[" ^ string_of_int (!shell_num) ^ "]" in
	let lexbuf = Lexing.from_channel stdin in
	lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = input_name };

	(* Get a single toplevel sequence using the "repl" start symbol *)
	Parser.repl (Lexer.main) lexbuf

(* notify [event -> unit] [private]
   Reports an event in execution (successful binding, type definitions, etc) in
   the interactive shell *)
(* TODO: Use confid to decide the boolean in repr_value (detailed output) *)
let notify event = match event with
	| Ev_Result value ->
		printf "- = %s\n" (repr_value value false)
	| Ev_Binding (name, value) ->
		printf "%s = %s\n" name (repr_value value false)

(* shell_repl [config -> env -> env] [private]
   The promised Read-Eval-Print Loop. Returns the environment at the end of
   input *)
let rec shell_repl conf env =
	incr shell_num;
	printf "[%d]: " !shell_num;
	flush stdout;

	let stmts = get_command () in

	(* execute_stmt [statement -> env -> env]
	   Executes a statement, handles the side-effects, and returns the
	   resulting environment *)
	let execute_stmt stmt env =
		let (newenv, events) = interpreter_exec env stmt in
		List.iter notify events;
		newenv in

	let newenv = List.fold_left execute_stmt env stmts in
	if stmts = [] then env else shell_repl conf newenv

(* shell_main [config -> unit]
   Starts an interactive REPL session. Returns when the user leaves the
   interpreter *)
let shell_main conf =
	print_string
		"  This is an interactive Fouine shell. Type Ctrl-D to leave.\n\n";
	let env = {
		vars  = StringMap.empty;
		types = StringMap.empty;
	} in
	let _ = shell_repl conf env in ()