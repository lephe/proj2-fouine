(*
**	Lexer - An ocamllex-generated lexer
*)

{
open Parser

let comment_depth = ref 0
}

(* Lexing rules for the main program - expressions *)
rule main = parse

	(* Ignore whitespaces *)
	| [' ' '\t']+ { main lexbuf }

	(* Record newlines in the lexbuf position attribute *)
	| '\n' { Lexing.new_line lexbuf; main lexbuf }

	(* Start of comments. To lex them I use a different "context function" *)
	| "(*" { comment_depth := 1; comment lexbuf; main lexbuf }

	(* Punctuation *)
	| '('	{ LPAR }
	| ')'	{ RPAR }
	| ';'	{ SEMI }
	| ";;"	{ SEMISEMI }
	| "->"	{ ARROW }
	| '_'	{ UND }
	| ":="	{ ASSIGN }
	| '!'	{ BANG }
	| ','	{ COMMA }
	| '|'	{ PIPE }
	| '['	{ LBRACK }
	| ']'	{ RBRACK }
	| "::"	{ CONS }

	(* Operators *)
	| '+'	{ PLUS }
	| '-'	{ MINUS }
	| '*'	{ TIMES }
	| '/'	{ DIV }
	| '='	{ EQ }
	| "<>"	{ NE }
	| '<'	{ LT }
	| '>'	{ GT }
	| "<="	{ LE }
	| ">="	{ GE }

	(* Operator-like things that should not be broken up!
	   This construct imitates OCaml's user-defined operators such as "1 ++ 2".
	   I don't support user-defined operators, but I still need to detect them
	   because they could accidentally be parsed in a wrong way, such as "--2"
	   being read as "-(-(2))" *)
	| ['+' '-' '!'] ['+' '-' '!']+ as op { OPERATOR op }

	(* Keywords *)
	| "begin"	{ BEGIN }
	| "else"	{ ELSE }
	| "end"		{ END }
	| "fun"		{ FUN }
	| "if"		{ IF }
	| "in"		{ IN }
	| "let"		{ LET }
	| "match"	{ MATCH }
	| "rec"		{ REC }
	| "ref"		{ REF }
	| "then"	{ THEN }
	| "try"		{ TRY }
	| "type"	{ TYPE }
	| "with"	{ WITH }

	(* Literals - literal unit is "LPAR RPAR" and is built by the parser *)
	| ['0' - '9']+ as s { INT (int_of_string s) }
	| "true"  { BOOL true }
	| "false" { BOOL false }
	| ['a' - 'z' '_'] ['a' - 'z' 'A' - 'Z' '_' '\'' '0' - '9']* as s { NAME s }

	(* Constructors have this uppercase style *)
	| ['A' - 'Z' '_'] ['a' - 'z' 'A' - 'Z' '_' '\'' '0' - '9']* as s { CTOR s }


	(* End Of File *)
	| eof { EOF }

(* Lexing rules for recursive OCaml-style comments *)
and comment = parse
	| "(*" { incr comment_depth; comment lexbuf }
	| "*)" {
		decr comment_depth;
		if !comment_depth = 0 then () else comment lexbuf
	}

	(* I'm using the "choose-the-longest-match" behavior to make the following
	   rule less priority than the previous two *)
	| [ ^ '(' '*' ')' ]+ | '(' | '*' | ')' { comment lexbuf }
