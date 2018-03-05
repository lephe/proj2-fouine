(*
**	Lexer - An ocamllex-generated lexer for the fouine language
*)

{
open Parser
}

rule token = parse

	(* Ignore whitespaces *)
	| [' ' '\t']+ { token lexbuf }

	(* Record newlines *)
	| '\n' { Lexing.new_line lexbuf; token lexbuf }

	(* Punctuation *)
	| '('  { LPAR }
	| ')'  { RPAR }

(*	| ','  { COMMA }
	| "->" { ARROW }
	| ";;" { DSC }
*)

	(* Operators *)
	| '+'  { PLUS }
	| '-'  { MINUS }
	| '*'  { TIMES }
	| '='  { EQ }
	| "<>" { NE }
	| '<'  { LT }
	| '>'  { GT }
	| "<=" { LE }
	| ">=" { GE }

	(* Keywords *)
	| "let"   { LET }
	| "in"    { IN }

(*	| "rec"   { REC }
	| "begin" { BEGIN }
	| "end"   { END }
	| "fun"   { FUN }
	| "if"    { IF }
	| "then"  { THEN }
	| "else"  { ELSE }
*)
	(* Literals *)
	| '-'? ['0' - '9']+ as s { INT (int_of_string s) }
	| ['a' - 'z' '_'] ['a' - 'z' '_' '0' - '9']* as s { NAME s }

	(* End Of File *)
	| eof { EOF }
