(*
**	Parser - A Menhir-generated LR(1) parser for the fouine language
*)

%{
open Util
open Expr

(* Temporary types for operator tokens with an annotated range *)
type operator = range * string
type ast = range aexpr

(* Make integer nodes [range -> int -> ast] *)
let make_int  r i : ast = (r, Aexpr_LitInt i)
(* Make name nodes [range -> string -> ast] *)
let make_name r n : ast = (r, Aexpr_Name n)

(* Make unary operator nodes (range * int) -> ast -> ast *)
let make_uop r0 op e1 : ast =
	let r' = range_merge r0 (fst e1) in
	match op with
	| "-" -> (r', Aexpr_Minus e1)
	| _ -> failwith ("make_uop: Invalid operator name " ^ op)

(* Make binary operator nodes *)
let make_bop e1 op e2 : ast =
	let r' = range_merge (fst e1) (fst e2) in
	match op with
	| "+" -> (r', Aexpr_Sum (e1, e2))
	| "-" -> (r', Aexpr_Diff (e1, e2))
	| "*" -> (r', Aexpr_Prod (e1, e2))
	| _ -> failwith ("make_bop: Invalid operator name " ^ op)

(* Make let nodes *)
let make_let r0 n e f : ast =
	let r' = range_merge (range_merge r0 (fst e)) (fst f) in
	(r', Aexpr_Let (n, e, f))

%}

/* Literals */
%token <int> INT
%token <string> NAME
/* Punctuation - DSC means double semicolon (;;) */
%token LPAR RPAR /* COMMA ARROW DSC */
/* Operators */
%token PLUS MINUS TIMES
%token GT GE LT LE EQ NE
/* Keywords */
%token LET IN /* REC BEGIN END FUN IF THEN ELSE */
/* Special end-of-file token for end-of-stream conflicts */
%token EOF

/*
	Precedence relations (I'm reproducing those of OCaml here!)
*/

%nonassoc IN
/*
%nonassoc IF THEN ELSE
%right ARROW
*/
%left EQ NE
%left GT GE LT LE
%left PLUS MINUS
%left TIMES
%left prec_UMINUS		/* This is not a true token, just a symbol */

/* Our CST will be annotated with line and column numbers */
%start <Util.range Expr.aexpr> program

%%

program:
	| e = expr EOF { e }

expr:
	/* Literal integers or variable accesses */
	| i = INT  { make_int  ($startpos, $endpos) i }
	| n = NAME { make_name ($startpos, $endpos) n }

	/* Parentheses */
	| LPAR e = expr RPAR
		{ (($startpos, $endpos), snd e) }

	/* let .. in rule */
	| LET n = NAME EQ e = expr IN f = expr
		{ make_let ($startpos, $endpos) n e f }

	/* Usual binary operators */
	| e = expr PLUS  f = expr { make_bop e "+"  f }
	| e = expr MINUS f = expr { make_bop e "-"  f }
	| e = expr TIMES f = expr { make_bop e "*"  f }
	| e = expr EQ    f = expr { make_bop e "="  f }
	| e = expr NE    f = expr { make_bop e "<>" f }
	| e = expr GT    f = expr { make_bop e ">"  f }
	| e = expr GE    f = expr { make_bop e ">=" f }
	| e = expr LT    f = expr { make_bop e "<"  f }
	| e = expr LE    f = expr { make_bop e "<=" f }

	/* Rule with special precedence: unary minus */
	| MINUS e = expr %prec prec_UMINUS
		{ make_uop ($startpos, $endpos) "-" e }
