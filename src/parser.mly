(*
**	Parser - A Menhir-generated LR(1) parser for the fouine language
*)

%{
open Types
open Util
open Exceptions

(* The syntax tree will just be an expression *)
type ast = expr
(* Unstructured toplevel declarations *)
type toplevel =
	| Let of range * bool * pattern * pattern list * expr
	| Type of range * string * string list

(*
**	Node construction functions
**	The following functions build the nodes of the tree. Some take a range
**	object as parameter; it is omitted only when it can be deduced from the
**	parameter metadata (ie, when both the first and the last token of the
**	rule are already of the type ast).
*)

(* Make integer nodes [range -> int -> ast] *)
let make_int  r i : ast = { range = r; tree = LiteralInt i }

(* Make more fuuun o/ (this function curries function definitions) *)
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
let make_ifte r e t fo : ast =
	(* Set a default condition, unit, for the else if none is specified *)
	let f = match fo with
	| Some f -> f
	| None -> { range = (snd r, snd r); tree = LiteralUnit }
	in { range = r; tree = If(e, t, f) }

(* Make unary operator nodes [range -> string -> ast -> ast] *)
let make_unary r0 op e1 : ast =
	let r = range_merge r0 e1.range in
	match op with
	| "+" -> { range = r; tree = UPlus e1 }
	| "-" -> { range = r; tree = UMinus e1 }
	| "!" -> { range = r; tree = Bang e1 }
	| _ -> raise (InternalError
		("The parser suddenly forgot what " ^ op ^ " means x_x\n"))

(* Make binary operator nodes [ast -> string -> ast -> ast] *)
let make_binary e1 op e2 : ast =
	let r = range_merge e1.range e2.range in
	match op with
	| "+"  -> { range = r; tree = Plus			(e1, e2) }
	| "-"  -> { range = r; tree = Minus			(e1, e2) }
	| "*"  -> { range = r; tree = Times			(e1, e2) }
	| "/"  -> { range = r; tree = Divide		(e1, e2) }
	| "="  -> { range = r; tree = Equal			(e1, e2) }
	| "<>" -> { range = r; tree = NotEqual		(e1, e2) }
	| ">"  -> { range = r; tree = Greater		(e1, e2) }
	| ">=" -> { range = r; tree = GreaterEqual	(e1, e2) }
	| "<"  -> { range = r; tree = Lower			(e1, e2) }
	| "<=" -> { range = r; tree = LowerEqual	(e1, e2) }
	| _ -> raise (InternalError
		("The parser suddenly forgot what " ^ op ^ " means x_x\n"))

(* Make programs out of unstructured let lists [toplevel list -> ast] *)
let rec make_toplevel (decls: toplevel list) final : ast =
	match decls with
	| [] -> final
	| Let (r, recursive, pat, args, e) :: tail ->
		let f = make_toplevel tail final in
		if args = []
			then make_letv r pat e f
			else make_letf r recursive pat args e f
	| Type (r, name, ctors) :: tail ->
		let f = make_toplevel tail final in
		{ range = r; tree = TypeDecl (name, ctors, f) }

(* An empty list [range -> ast] *)
let make_list_empty r : ast =
	let (b, _) = r in
	let nothing = { range = (b, b); tree = LiteralUnit } in
	{ range = r; tree = ExprCtor ("Empty", nothing) }

(* A list constructor [range -> expr -> expr -> ast] *)
let make_list_cons r hd tl : ast =
	let pair = { range = r; tree = ExprTuple [hd; tl] } in
	{ range = r; tree = ExprCtor ("Cons", pair) }

%}

/*
**	Token list
*/

/* Literals */
%token <int> INT
%token <bool> BOOL
%token <string> NAME
%token <string> CTOR

/* Punctuation */
%token LPAR RPAR SEMI SEMISEMI
%token ARROW UND ASSIGN BANG COMMA PIPE
%token LBRACKET RBRACKET CONS

/* Operators */
%token PLUS MINUS TIMES DIV
%token GT GE LT LE EQ NE

/* Token for unknown constructs that resemble (user-defined) operators */
%token <string> OPERATOR

/* Keywords */
%token LET REC IN
%token BEGIN END
%token IF THEN ELSE
%token FUN
%token REF
%token TYPE MATCH WITH

/* Special end-of-file token for end-of-stream conflicts */
%token EOF

/*
**	Precedence relations (mainly reproducing those of OCaml)
*/

/* Semicolon, for instruction sequences */
%nonassoc below_SEMI
%right SEMI
/* THEN has to be strictly below ELSE, otherwise the "if..then" rule would
   always be reduced regardless of whether there is an "else" clause */
%nonassoc THEN
%nonassoc ELSE
/* Testing reveals that assignment is around here */
%right ASSIGN
/* This precedence, among with WITH, solves conflicts in match cases */
%nonassoc below_PIPE
%left PIPE
/* Comma is non-associative because a, b, c != (a, b), c != a, (b, c)! */
%nonassoc below_COMMA
%left COMMA
/* ARROW has a low priority so that fun x -> x + 2 means fun x -> (x + 2) */
%right ARROW
/* OCaml says that EQ, GT and LT have the same priority, but I have found
   nothing in the parser about NE, GE and LE. A bit of experimentation
   suggested that they all have the same precedence level */
%left EQ NE GT GE LT LE
/* List constructors */
%right CONS
/* Usual arithmetic operations, and unknown (user-defined) operators */
%left PLUS MINUS OPERATOR
%left TIMES DIV
/* ThesE are not true tokens, just dummy symbols used within %prec rules */
%left prec_UPLUS prec_UMINUS
/* As in the original OCaml parser, the tokens that introduce simple
   expressions have the highest priority, so that we always shift them. This
   happens especially in cases like "MINUS NAME . INT" where we want to shift
   the integer to reduce the function call before reducing the unary minus */
%nonassoc BEGIN INT BOOL NAME LPAR BANG LBRACKET

/* Our CST will be annotated with line and column numbers */
%start <Types.expr> toplevel
/* Give some types to improve error messages from type inference */
%type <toplevel> toplevel_decl
%type <Types.expr> expr_list

%%

/*
**  Toplevel expressions
*/

toplevel:
	/* Specification says that in the absence of a final expression, 0 is
	   used in place */
	| SEMISEMI EOF | EOF { make_int ($startpos, $endpos) 0 }
	| e = seq_expr EOF { e }

	/* The following rules allow the use of ";;" and omitting the "in" at
	   the root of the source tree (following Ocaml syntax) */
	| seq = toplevel_seq SEMISEMI p = toplevel
		{ make_toplevel seq p }

	/* Omitting even the ";;" is allowed if there is no final expression */
	| seq = toplevel_seq EOF
		{ make_toplevel seq (make_int ($endpos, $endpos) 0) }

	/* The following rule allows writing bare expressions before a ";;". The
	   string "expr;;" resolves to "let _ = expr;;" */
	| e = seq_expr SEMISEMI p = toplevel
		{ make_toplevel [ Let (e.range, false, Wildcard, [], e) ]  p }

toplevel_seq:
	/* I had to use Menhir's standard lists at least once */
	| seq = nonempty_list(toplevel_decl) { seq }

toplevel_decl:
	/* Two flavours of let bindings - see expr for more detail */
	| LET recursive = boption(REC) pat = pattern EQ e = expr
		{ Let (($startpos, $endpos), false, pat, [], e) }
	| LET recursive = boption(REC) n = NAME args = nonempty_list(pattern) EQ
	  e = expr
		{ Let (($startpos, $endpos), recursive, Identifier n, args, e) }
	/* Type declarations are only found at the toplevel */
	| TYPE n = NAME EQ option(PIPE) hd = CTOR l = list(PIPE c = CTOR { c })
		{ Type (($startpos, $endpos), n, hd :: l) }

/*
**  General expressions
*/

seq_expr:
	/* Make the rule greedy - see somewhere below */
	| e = expr %prec below_SEMI { e }
	| e = expr SEMI f = seq_expr
		{ make_letv ($startpos, $endpos) Wildcard e f }

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
	| LET recursive = boption(REC) pat = pattern EQ e = seq_expr IN
	  f = seq_expr
		{ make_letv ($startpos, $endpos) pat e f }

	/* Function bindings. The name must not be a pattern, thus the need for two
	   productions. The argument list must not be empty to avoid conflicts with
	   the previous production */
	| LET recursive = boption(REC) n = NAME args = nonempty_list(pattern) EQ
	  e = seq_expr IN f = seq_expr
		{ make_letf ($startpos, $endpos) recursive (Identifier n) args e f }

	/* Pattern matching */
	| MATCH e = seq_expr WITH option(PIPE) ps = match_cases
		{ { range = ($startpos, $endpos); tree = Match (e, ps) } }

	/* Reference assignments */
	| e = expr ASSIGN f = expr
		{ { range = range_merge e.range f.range; tree = Assign (e, f) } }

	/* Conditions
	   Not any possibility of using Menhir's option() here (AFAIK) because the
	   two rules have different prioritIes - ELSE being higher than THEN */
	| IF e = seq_expr THEN t = expr ELSE f = expr
		{ make_ifte ($startpos, $endpos) e t (Some f) }
	| IF e = seq_expr THEN t = expr
		{ make_ifte ($startpos, $endpos) e t None }

	/* Function applications */
	| e = expr se = simple_expr
		{ { range = range_merge e.range se.range; tree = Call (e, se) } }
	/* "ref" is also a function call, priority-wise */
	| REF se = simple_expr
		{ { range = ($startpos, $endpos); tree = Ref se } }
	/* And so are constructors */
	| c = CTOR se = simple_expr
		{ { range = ($startpos, $endpos); tree = ExprCtor (c, se) } }

	/* Function definitions */
	| FUN pl = nonempty_list(pattern) ARROW e = seq_expr
		{ make_fun ($startpos, $endpos) pl e }

	/* Usual arithmetic operations */
	| e = expr PLUS  f = expr	{ make_binary e "+" f }
	| e = expr MINUS f = expr	{ make_binary e "-" f }
	| e = expr TIMES f = expr	{ make_binary e "*" f }
	| e = expr DIV   f = expr	{ make_binary e "/" f }

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

	/* List const */
	| e = expr CONS f = expr
		{ make_list_cons ($startpos, $endpos) e f }

	/* Rule with special precedence: unary plus and unary minus */
	| PLUS  e = expr %prec prec_UPLUS
		{ make_unary ($startpos, $endpos) "+" e }
	| MINUS e = expr %prec prec_UMINUS
		{ make_unary ($startpos, $endpos) "-" e }

	/* A precedence below that of COMMA prevents this rule from being reduced
	   as long as a comma is available as a lookahead, making it, in practice,
	   greedy */
	| ecl = expr_comma_list %prec below_COMMA
		{ { range = ($startpos, $endpos); tree = ExprTuple (List.rev ecl) } }

expr_comma_list:
	/* Aggregates, tuples, etc. I need to specify at least one element
	   otherwise the list would have a production expr -> expr */
	| e = expr COMMA f = expr { [f; e] }
	| ecl = expr_comma_list COMMA g = expr { g :: ecl }

simple_expr:
	/* Literal integers or variable names, units */
	| i = INT   { make_int  ($startpos, $endpos) i }
	| b = BOOL  { { range = ($startpos, $endpos); tree = LiteralBool b } }
	| n = NAME  { { range = ($startpos, $endpos); tree = Name n } }
	| LPAR RPAR { { range = ($startpos, $endpos); tree = LiteralUnit } }

	/* Expressions enclosed within parentheses or begin..end */
	| LPAR e = expr RPAR
	| BEGIN e = expr END
		{ { range = e.range; tree = e.tree } }

	/* Dereferencing */
	| BANG se = simple_expr
		{ make_unary ($startpos, $endpos) "!" se }

	/* Lists */
	| LBRACKET e = expr_list RBRACKET { e }

expr_list:
	| /* empty */
		{ make_list_empty ($startpos, $endpos) }
	| e = expr
		{ let r = ($startpos, $endpos) in
			make_list_cons r e (make_list_empty r) }
	| e = expr SEMI l = expr_list
		{ make_list_cons ($startpos, $endpos) e l }

/*
**  Match cases, for functional pleasure
*/

/* I can't use Menhir's standard library's separated_nonempty_list() becauses
   it's not inline and it "hides" the rule precedence, which is that of PIPE */
match_cases:
	/* Same hack to make the rule greedy - I lack time do it 100% properly */
	| c = match_case %prec below_PIPE { [ c ] }
	| c = match_case PIPE cl = match_cases { c :: cl }

match_case:
	| p = pattern ARROW e = expr { (p, e) }

/*
**  Binding patterns for use by let expressions and function definitions
*/

pattern:
	/* Simple patterns - same "trick" as for expressions */
	| sp = simple_pattern { sp }
	/* ADT constructors */
	| c = CTOR sp = simple_pattern { PatternCtor (c, sp) }
	/* Tuples - same technique as the tuple expressions above */
	| pcl = pattern_comma_list %prec below_COMMA { Product (List.rev pcl) }
	/* List constructors, which is a binary operator */
	| p = pattern CONS q = pattern { PatternCtor ("Cons", Product [p; q]) }

simple_pattern:
	/* Basic patterns: variable names and the wildcard */
	| n = NAME		{ Identifier n }
	| UND			{ Wildcard }
	/* Literal values (must match exacly) */
	| i = INT		{ PatternInt i }
	| b = BOOL		{ PatternBool b }
	| LPAR RPAR		{ PatternUnit }
	/* Parentheses may also appear in patterns */
	| LPAR p = pattern RPAR { p }
	/* List patterns */
	| LBRACKET p = pattern_list RBRACKET { p }

pattern_list:
	| /* empty */
		{ PatternCtor ("Empty", PatternUnit) }
	| p = pattern
		{ let last = PatternCtor ("Empty", PatternUnit) in
		  PatternCtor ("Cons", Product [p; last]) }
	| p = pattern SEMI pl = pattern_list
		{ PatternCtor ("Cons", Product [p; pl]) }

pattern_comma_list:
	| p = pattern COMMA q = pattern { [q; p] }
	| pcl = pattern_comma_list COMMA r = pattern { r :: pcl }
