open Util
open Expr

let parse_file f =
	let channel = open_in f in
	let lexbuf = Lexing.from_channel channel in
	lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = f };
	let ast = Parser.program (Lexer.token) lexbuf in
	close_in channel;
	ast

let _ =
	if Array.length Sys.argv < 2 then
		print_string "filename expected!\n"
	else
		let ast = parse_file Sys.argv.(1) in
		aexpr_print range_str 0 ast

let any_function_name_here =
	let line = "let x = undefined + 3 in x * 2\n"
	and range = (0, 8, 9) in
	range_highlight line range
