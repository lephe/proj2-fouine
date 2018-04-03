(*
**  REPL - A Read-Eval-Print Loop shell for Fouine
*)

open Types
open Eval
open Expr
open Toplevel

let shell_num = ref 0

(* get_command [unit -> decl list]
   Reads and parses a command from standard input *)
let get_command () =
	(* Start the lexer with the command number as file name *)
	let input_name = "[" ^ string_of_int (!shell_num) ^ "]" in
	let lexbuf = Lexing.from_channel stdin in
	lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = input_name };

	(* Get a single toplevel sequence using the "repl" start symbol *)
	Parser.repl (Lexer.main) lexbuf

(* notify_var [string -> value -> unit]
   The callback that prints bindings in the shell, during evaluation *)
let notify_var (n, v) =
	Printf.printf "%s = %s\n" n (value_str v)

(* notify_type [string -> string list -> unit]
   The callback that prints type definitions in the shell *)
let notify_type (name, ctors) =
	(* TODO: Notify type definitions in the shell *)
	()

let rec shell_repl conf env =
	incr shell_num;
	Printf.printf "[%d]: " !shell_num;
	flush stdout;

	let decls = get_command () in
	let newenv = List.fold_left (top_exec notify_var notify_type) env decls in

	if decls = [] then () else shell_repl conf newenv

let shell_main conf =
	print_string
		"  This is an interactive Fouine shell. Type Ctrl-D to leave.\n\n";
	let env = {
		vars  = StringMap.empty;
		types = StringMap.empty;
	} in
	shell_repl conf env
