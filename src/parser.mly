(*
**	Parser - A Menhir-generated LR(1) parser for the fouine language
*)

%{
open Types
open Util
open Exceptions

(* TODO: Try to detect parse errors (and lexing errors by the way), and set up
   these exceptions in exceptions.ml *)
exception ParseError

(* The syntax tree will just be an expression *)
type ast = expr
(* Unstructured toplevel lets use this type until make_toplevel is called *)
type toplet = range * bool * pattern * pattern list * expr

(*
**	Node construction functions
**	The following functions build the nodes of the tree. Some take a range
**	object as parameter; it is omitted only when it can be deduced from the
**	parameter metadata (ie, when both the first and the last token of the
**	rule are already of the type ast).
*)

(* Make integer nodes [range -> int -> ast] *)
let make_int  r i : ast = { range = r; tree = LiteralInt i }
(* Make bool literals [range -> bool -> ast] *)
let make_bool r b : ast = { range = r; tree = LiteralBool b }
(* Make unit nodes [range -> ast] *)
let make_unit r   : ast = { range = r; tree = LiteralUnit }
(* Make name nodes [range -> string -> ast] *)
let make_name r n : ast = { range = r; tree = Name n }

(* Make unary operator nodes [range -> string -> ast -> ast] *)
let make_unary r0 op e1 : ast =
	let r = range_merge r0 e1.range in
	match op with
	| "+" -> { range = r; tree = UPlus e1 }
	| "-" -> { range = r; tree = UMinus e1 }
	| _ -> raise (InternalError
		("The parser suddenly forgot what " ^ op ^" means x_x\n"))

(* Make binary operator nodes [ast -> string -> ast -> ast] *)
let make_binary e1 op e2 : ast =
	let r = range_merge e1.range e2.range in
	match op with
	| "+"  -> { range = r; tree = Plus			(e1, e2) }
	| "-"  -> { range = r; tree = Minus			(e1, e2) }
	| "*"  -> { range = r; tree = Times			(e1, e2) }
	| "="  -> { range = r; tree = Equal			(e1, e2) }
	| "<>" -> { range = r; tree = NotEqual		(e1, e2) }
	| ">"  -> { range = r; tree = Greater		(e1, e2) }
	| ">=" -> { range = r; tree = GreaterEqual	(e1, e2) }
	| "<"  -> { range = r; tree = Lower			(e1, e2) }
	| "<=" -> { range = r; tree = LowerEqual	(e1, e2) }
	| _ -> raise (InternalError
		("The parser suddenly forgot what " ^ op ^" means x_x\n"))

(* Make function calls *)
let make_call e1 e2 : ast =
	let r = range_merge e1.range e2.range in
	{ range = r; tree = Call (e1, e2) }

(* Make more fuuun o/ (this function curries function definitions)
   TODO: make_fun: should we use different ranges for each argument? *)
let rec make_fun r args e : ast = match args with
	| [] -> e
	| arg :: tl -> { range = r; tree = Function (arg, make_fun r tl e) }

(* Make let-value nodes [range -> pattern -> ast -> ast -> ast] *)
let make_letv r pat e f : ast =
	{ range = r; tree = Let (false, pat, e, f) }

(* Make let-function nodes
   [range -> bool -> pattern -> pattern list -> ast -> ast -> ast]
   The first pattern is expected to be "Identifier name" (if not, a type error
   will be raised at runtime when matching the function with the pattern) *)
let make_letf r recursive pat args e f : ast =
	(* For functions the let-binding must be a single name *)
	{ range = r; tree = Let (recursive, pat, make_fun r args e, f) }

(* Make if-then-else nodes *)
let make_ifte r e t fo =
	(* Set a default condition, unit, for the else if none is specified *)
	let f = match fo with
	| Some f -> f
	| None -> { range = (snd r, snd r); tree = LiteralUnit }
	in { range = r; tree = If(e, t, f) }

(* Make programs out of unstructured let lists *)
let rec make_toplevel (lets: toplet list) final = match lets with
	| [] -> final
	| (r, recursive, pat, args, e) :: tail ->
		let f = make_toplevel tail final in
		if args = []
			then make_letv r pat e f
			else make_letf r recursive pat args e f

%}

/*
**	Token list
*/

/* Literals */
%token <int> INT
%token <bool> BOOL
%token <string> NAME

/* Punctuation - DSC means double semicolon (;;) */
%token LPAR RPAR DSC ARROW UND /* COMMA */

/* Operators */
%token PLUS MINUS TIMES
%token GT GE LT LE EQ NE

/* Token for unknown constructs that resemble (user-defined) operators */
%token <string> OPERATOR

/* Keywords */
%token LET REC IN
%token BEGIN END
%token IF THEN ELSE
%token FUN

/* Special end-of-file token for end-of-stream conflicts */
%token EOF

/*
**	Precedence relations (mainly reproducing those of OCaml)
*/

/* Nothing is "larger" than a let..in statement, hence IN structures the
   program at the outermost level */
%nonassoc IN
/* THEN is stricly below ELSE, otherwise the "if..then" rule would always be
   reduced regardless of whether there is an "else" clause */
%nonassoc THEN
%nonassoc ELSE
/* ARROW has a low priority so that fun x -> x + 2 means fun x -> (x + 2) */
%right ARROW
/* OCaml says that EQ, GT and LT have the same priority, but I have found
   nothing in the parser about NE, GE and LE. A bit of experimentation
   suggested that they all have the same precedence level */
%left EQ NE GT GE LT LE
/* Usual arithmetic operations, and unknown (user-defined) operators */
%left PLUS MINUS OPERATOR
%left TIMES
/* Thes are not true tokens, just dummy symbols used within %prec rules */
%left prec_UPLUS prec_UMINUS
/* As in the original OCaml parser, the tokens that introduce simple
   expressions have the highest priority, so that we always shift them. This
   happens especially in cases like "MINUS NAME . INT" where we want to shift
   the integer to reduce the function call before reducing the unary minus */
%nonassoc BEGIN INT BOOL NAME LPAR

/* Our CST will be annotated with line and column numbers */
%start <Types.expr> toplevel
/* Give some types to improve error messages from type inference */
%type <toplet> toplevel_binding

%%

/*
**  Toplevel expressions
*/

toplevel:
	/* Specification says that in the absence of a final expression, 0 is
	   used in place */
	| EOF { make_int ($startpos, $endpos) 0 }
	| e = expr EOF { e }

	/* The following rules allow the use of ";;" and omitting the "in" at
	   the root of the source tree (following Ocaml syntax) */
	| seq = toplevel_seq DSC p = toplevel
		{ make_toplevel seq p }

	/* Omitting even the ";;" is allowed if there is no final expression */
	| seq = toplevel_seq EOF
		{ make_toplevel seq (make_int ($endpos, $endpos) 0) }

	/* The following rule allows writing bare expressions before a ";;". The
	   string "expr;;" resolves to "let _ = expr;;" */
	| e = expr DSC p = toplevel
		/* TODO: Make a special pattern constructor for "_" */
		{ make_toplevel [ (e.range, false, Identifier "_", [], e) ]  p }

toplevel_seq:
	/* I had to use Menhir's standard lists at least once */
	| seq = nonempty_list(toplevel_binding) { seq }

toplevel_binding:
	/* Two flavours of let bindings - see expr for more detail */
	| LET recursive = boption(REC) pat = pattern EQ e = expr
		{ (($startpos, $endpos), false, pat, [], e) }
	| LET recursive = boption(REC) n = NAME args = nonempty_list(pattern) EQ
	  e = expr
		{ (($startpos, $endpos), recursive, Identifier n, args, e) }

/*
**  General expressions
*/

expr:
	/* Simple expressions are literals and enclosed expressions - roughly the
	   things that can be passed to the highest-priority operator, function
	   application, without triggering a syntax error */
	| se = simple_expr { se }

	/* Let bindings - come in two flavours:
	     let <pattern> = <expr> in <expr>
	     let [rec] <function-name> <pattern...> = <expr> in <expr>
	   The productions actually allow rec to be used for value bindings, but it
	   is ignored (for now). This avoids some ambiguities */
	/* TODO: Handle rec for value bindings */

	/* Value bindings. Note that bound values are expressions, not just
	   literals. They will be unified with the pattern at runtime */
	| LET recursive = boption(REC) pat = pattern EQ e = expr IN f = expr
		{ make_letv ($startpos, $endpos) pat e f }

	/* Function bindings. The name must not be a pattern, thus the need for two
	   productions. The argument list must not be empty to avoid conflicts with
	   the previous production */
	| LET recursive = boption(REC) n = NAME args = nonempty_list(pattern) EQ
	  e = expr IN f = expr
		{ make_letf ($startpos, $endpos) recursive (Identifier n) args e f }

	/* Conditions
	   Not any possibility of using Menhir's option() here (AFAIK) because the
	   two rules have different priorites - ELSE being higher than THEN */
	| IF e = expr THEN t = expr ELSE f = expr
		{ make_ifte ($startpos, $endpos) e t (Some f) }
	| IF e = expr THEN t = expr
		{ make_ifte ($startpos, $endpos) e t None }

	/* Function applications */
	| e = expr se = simple_expr
		{ make_call e se }

	/* Function definitions */
	| FUN pl = nonempty_list(pattern) ARROW e = expr
		{ make_fun ($startpos, $endpos) pl e }

	/* Usual arithmetic operations */
	| e = expr PLUS  f = expr	{ make_binary e "+"  f }
	| e = expr MINUS f = expr	{ make_binary e "-" f }
	| e = expr TIMES f = expr	{ make_binary e "*" f }

	/* User-defined operators - just complain!
	   This rule exists to avoid "1+-2" being accepted by the parser */
	| e = expr op = OPERATOR f = expr
		{ raise (InvalidOperator op) }

	/* Comparisons */
	| e = expr EQ f = expr		{ make_binary e "="  f }
	| e = expr NE f = expr		{ make_binary e "<>" f }
	| e = expr GT f = expr		{ make_binary e ">"  f }
	| e = expr GE f = expr		{ make_binary e ">=" f }
	| e = expr LT f = expr		{ make_binary e "<"  f }
	| e = expr LE f = expr		{ make_binary e "<=" f }

	/* Rule with special precedence: unary plus and unary minus */
	| PLUS  e = expr %prec prec_UPLUS
		{ make_unary ($startpos, $endpos) "+" e }
	| MINUS e = expr %prec prec_UMINUS
		{ make_unary ($startpos, $endpos) "-" e }

simple_expr:
	/* Literal integers or variable names, units */
	| i = INT   { make_int  ($startpos, $endpos) i }
	| b = BOOL  { make_bool ($startpos, $endpos) b }
	| n = NAME  { make_name ($startpos, $endpos) n }
	| LPAR RPAR { make_unit ($startpos, $endpos) }

	/* Expressions enclosed within parentheses or begin..end */
	| LPAR e = expr RPAR
	| BEGIN e = expr END
		{ { range = ($startpos, $endpos); tree = e.tree } }

/*
**  Binding patterns for use by let expressions and function definitions
*/

pattern:
	/* TODO: Support more advanced types of patterns (pairs, etc) */
	| n = NAME { Identifier n }
	| UND { Wildcard }
