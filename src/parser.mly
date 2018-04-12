(*
**	Parser - A Menhir-generated LR(1) parser
*)

(* This is an m4 file. See menhir.sh for the details of how it is preprocessed
   before being passed to Menhir.
   Each time a production is reduced, "($startpos, $endpos)" captures the
   location in the source where the reduced tokens are located. I use it a lot
   to be able to highlight the source code when errors happen.
   At some point this string started to appear everywhere and made the file a
   lot less readable, so I decided to use a bit of preprocessing magic *)
define(POS,($startpos, $endpos))

%{
open Types
open Range
open Exceptions

(* The syntax tree will just be an expression *)
type ast = expr

(*
**	Node construction functions
**	The following functions build the nodes of the tree. Some take a range
**	object as parameter; it is omitted only when it can be deduced from the
**	parameter metadata (ie, when both the first and the last token of the
**	rule are already ASTs).
**
**	The % operator is used below to affix trees with ranges, making expressions
**	(see "types.ml" for details). I often write "r% tree" or "POS% tree" to
**	keep the syntax light.
*)

let (%) range tree = { range = range; tree = tree }

(* Make "fun args... ->" nodes by currying *)
let rec make_fun r args e : ast = match args with
	| [] -> e
	| arg :: tl -> r% E_Function (arg, make_fun r tl e)

(* Make if-then nodes [range -> expr -> expr -> ast] *)
let make_ifthen r e t : ast =
	(* Set a default condition, unit, for the else clause *)
	let else_clause = (snd r, snd r) % E_Unit
	in r% E_If (e, t, else_clause)

(* Make unary operator nodes [range -> string -> ast -> ast] *)
let make_unary r0 op e1 : ast =
	let r = range_merge r0 e1.range in
	r% match op with
	| "+" -> E_UPlus e1
	| "-" -> E_UMinus e1
	| "!" -> E_Bang e1
	| _ -> raise (InternalError
		("The parser suddenly forgot what " ^ op ^ " means x_x\n"))

(* Make binary operator nodes [ast -> string -> ast -> ast] *)
let make_binary e1 op e2 : ast =
	let r = range_merge e1.range e2.range in
	r% match op with
	| "+"  -> E_Plus			(e1, e2)
	| "-"  -> E_Minus			(e1, e2)
	| "*"  -> E_Times			(e1, e2)
	| "/"  -> E_Divide			(e1, e2)
	| "="  -> E_Equal			(e1, e2)
	| "<>" -> E_NotEqual		(e1, e2)
	| ">"  -> E_Greater			(e1, e2)
	| ">=" -> E_GreaterEqual	(e1, e2)
	| "<"  -> E_Lower			(e1, e2)
	| "<=" -> E_LowerEqual		(e1, e2)
	| _ -> raise (InternalError
		("The parser suddenly forgot what " ^ op ^ " means x_x\n"))

(* An empty list [range -> ast] *)
let make_list_empty r : ast =
	let nothing = (fst r, fst r) % E_Unit in
	r% E_Ctor ("Empty", nothing)

(* A list constructor [range -> expr -> expr -> ast] *)
let make_list_cons r hd tl : ast =
	r% E_Ctor ("Cons", r% E_Tuple [hd; tl])

(* A list with a single element [range -> expr -> ast] *)
let make_list_one r e : ast =
	make_list_cons r e (make_list_empty r)

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
%token LBRACK RBRACK CONS

/* Operators */
%token PLUS MINUS TIMES DIV
%token GT GE LT LE EQ NE

/* Unknown constructs that resemble (user-defined) operators */
%token <string> OPERATOR

/* Keywords */
%token LET REC IN
%token BEGIN END
%token IF THEN ELSE
%token FUN
%token REF
%token TYPE TRY MATCH WITH

/* Special end-of-file token for end-of-stream conflicts */
%token EOF

/*
**	Precedence relations
**	We agree that %prec's are tricky and I've tried to keep them minimal. I
**	consider them acceptable where the OCaml parser uses them. Otherwise,
**	there's an entry in my doc/bugs file.
*/

/* [below_SEMI] Used to make seq_expr greedy */
%nonassoc below_SEMI
%right SEMI

/* THEN has to be strictly below ELSE, otherwise the "if..then" rule would
   always be reduced regardless of whether there is an "else" clause */
%nonassoc THEN
%nonassoc ELSE

%right ASSIGN

/* [below_PIPE] Used to make match_cases greedy (for nested matches). Can
   probably be avoided */
/* TODO: parser: Get rid of the below_PIPE precedence level */
%nonassoc below_PIPE
%left PIPE

/* [below_COMMA] Makes the tuple rule greedy (again) */
%nonassoc below_COMMA
%left COMMA

/* ARROW has a low priority so that fun x -> x + 2 means fun x -> (x + 2) */
%right ARROW

/* OCaml says that EQ, GT and LT have the same priority, but I have found
   nothing in the parser about NE, GE and LE. A bit of experimentation
   suggested that they all have the same precedence level */
%left EQ NE GT GE LT LE

%right CONS

/* Usual arithmetic operations, and unknown (user-defined) operators */
%left PLUS MINUS OPERATOR
%left TIMES DIV

/* [prec_UPLUS] [prec_UMINUS] Just because the same symbol is used for two
   operators with different priorities */
%left prec_UPLUS prec_UMINUS

/* Tokens that introduce simple, "atomic" expressions have the highest
   priority because we don't want to break down an atomic expression. This
   makes like "-f 3" work because we want to shift 3 before reducing -f */
%nonassoc BEGIN INT BOOL NAME LPAR BANG LBRACK

/* Our CST will be flattened at the top level */
%start <Types.statement list> toplevel
/* The REPL shell mainly reads toplevel commands terminated by ";;" */
%start <Types.statement list> repl

/* The types help improve error messages from type inference */
%type <Types.statement> toplevel_statement
%type <Types.expr> expr_list

%%

/*
**	Toplevel expressions
*/

toplevel:
	| SEMISEMI EOF | EOF				{ [] }
	| e = seq_expr EOF					{ [S_Expr (POS, e)] }

	/* Allow using ";;" and omitting the "in" at the toplevel (OCaml syntax) */
	| seq = toplevel_seq SEMISEMI p = toplevel { seq @ p }
	| seq = toplevel_seq EOF { seq }

	/* Expressions before ";;" resolve to "let _ = expr;;" */
	| e = seq_expr SEMISEMI p = toplevel
		{ S_LetVal (e.range, P_Wildcard, e) :: p }

toplevel_seq:
	| s = nonempty_list(toplevel_statement) { s }

/* Toplevel statements include let bindings and type definitions */
toplevel_statement:
	/* Two flavours of let; see "expr" for more detail */
	| LET recursive = boption(REC) pat = pattern EQ e = expr
		{ S_LetVal (POS, pat, e) }
	| LET recursive = boption(REC) n = NAME args = nonempty_list(pattern) EQ
	  e = expr {
		if not recursive
		then S_LetVal (POS, P_Name n, make_fun POS args e)
		else S_LetRec (POS, n, make_fun POS args e)
	  }
	/* OCaml-style type definitions (but not compatible with typing) */
	| TYPE n = NAME EQ option(PIPE) hd = CTOR l = list(PIPE c = CTOR { c })
		{ S_Type (POS, n, hd :: l) }

/* The input of the REPL shell */
repl:
	| repl_terminator { [] }
	| e = seq_expr repl_terminator { [S_Expr (POS, e)] }
	| s = toplevel_seq repl_terminator { s }

/* REPL command terminators */
repl_terminator:
	| EOF { print_newline () }
	| SEMISEMI { }

/*
**	General expressions
*/

/* seq_expr is the largest expression nonterminal, the only that allows ";" */
seq_expr:
	/* The precedence level below that of SEMI prevents the rule from being
	   reduced as long as semicolons are available, making it "greedy" */
	| e = expr %prec below_SEMI			{ e }
	| e = expr SEMI f = seq_expr		{ POS% E_LetVal (P_Wildcard, e, f) }

expr:
	/* Atomic expressions are literals and enclosed expressions - roughly the
	   things that can be passed to the highest-priority operator, function
	   application, without triggering a syntax error */
	| ae = atomic_expr					{ ae }

	/* Let bindings - come in two flavours:
	     let <pattern> = <expr> in <expr>
	     let [rec] <function-name> <patterns...> = <expr> in <expr>
	   Note that LetVal handles non-recursive function definitions as values;
	   semantic differences only arise when using let rec */

	| LET boption(REC) pat = pattern EQ e = seq_expr IN f = seq_expr
										{ POS% E_LetVal (pat, e, f) }

	/* In function bindings, the we can only bind one name at a time */
	| LET recursive = boption(REC) n = NAME args = nonempty_list(pattern) EQ
	  e = seq_expr IN f = seq_expr {
		if not recursive
		then POS% E_LetVal (P_Name n, make_fun POS args e, f)
		else POS% E_LetRec (n, make_fun POS args e, f)
	  }

	/* Pattern matching */
	| MATCH e = seq_expr WITH option(PIPE) mc = match_cases
										{ POS% E_Match (e, mc) }

	/* Try .. catch statements */
	| TRY e = seq_expr WITH option(PIPE) mc = match_cases
										{ POS% E_Try (e, mc) }

	/* Reference assignments */
	| e = expr ASSIGN f = expr			{ POS% E_Assign (e, f) }

	/* Conditions - I can't use Menhir's option() here (AFAIK) because the two
	   reductions must have a different priority */
	| IF e = seq_expr THEN t = expr ELSE f = expr
										{ POS% E_If (e, t, f) }
	| IF e = seq_expr THEN t = expr		{ make_ifthen POS e t }

	/* Function applications, and, priority-wise, "ref" and constructors */
	| e = expr	ae = atomic_expr 		{ POS% E_Call (e, ae) }
	| REF		ae = atomic_expr 		{ POS% E_Ref ae }
	| c = CTOR	ae = atomic_expr 		{ POS% E_Ctor (c, ae) }

	/* Function definitions */
	| FUN pl = nonempty_list(pattern) ARROW e = seq_expr
										{ make_fun POS pl e }

	/* Usual arithmetic operations */
	| e = expr PLUS  f = expr			{ make_binary e "+" f }
	| e = expr MINUS f = expr			{ make_binary e "-" f }
	| e = expr TIMES f = expr			{ make_binary e "*" f }
	| e = expr DIV   f = expr			{ make_binary e "/" f }

	/* User-defined operators - just complain!
	   This rule exists to avoid "1+-2" being accepted by the parser */
	| e = expr op = OPERATOR f = expr	{ raise (InvalidOperator op) }

	/* Comparisons */
	| e = expr EQ f = expr				{ make_binary e "="  f }
	| e = expr NE f = expr				{ make_binary e "<>" f }
	| e = expr GT f = expr				{ make_binary e ">"  f }
	| e = expr GE f = expr				{ make_binary e ">=" f }
	| e = expr LT f = expr				{ make_binary e "<"  f }
	| e = expr LE f = expr				{ make_binary e "<=" f }

	/* List const */
	| e = expr CONS f = expr			{ make_list_cons POS e f }

	/* Rule with special precedence: unary plus and unary minus */
	| PLUS  e = expr %prec prec_UPLUS	{ make_unary POS "+" e }
	| MINUS e = expr %prec prec_UMINUS	{ make_unary POS "-" e }

	/* Like expr_seq, this rule needs to be greedy to avoid ambiguities */
	| ec = expr_comma %prec below_COMMA	{ POS% E_Tuple (List.rev ec) }

/* Aggregates, tuples... one element must be specified, otherwise the list
   would have a production expr -> expr */
expr_comma:
	| e = expr COMMA f = expr			{ [f; e] }
	| ec = expr_comma COMMA g = expr	{ g :: ec }

atomic_expr:
	/* Literal integers or variable names, unit */
	| i = INT  						 	{ POS% E_Int i }
	| b = BOOL  						{ POS% E_Bool b }
	| n = NAME  						{ POS% E_Name n }
	| LPAR RPAR 						{ POS% E_Unit }

	/* Expressions enclosed within parentheses or begin..end */
	| LPAR e = expr RPAR				{ e }
	| BEGIN e = expr END				{ e }

	/* Dereferencing */
	| BANG ae = atomic_expr				{ make_unary POS "!" ae }

	/* Lists */
	| LBRACK e = expr_list RBRACK		{ e }

expr_list:
	| /* empty */						{ make_list_empty POS }
	| e = expr							{ make_list_one POS e }
	| e = expr SEMI l = expr_list		{ make_list_cons POS e l }

/*
**	Match cases, for functional pleasure
*/

/* I can't use Menhir's standard library's separated_nonempty_list() because
   it's not inline and it "hides" the rule precedence, which is that of PIPE */
match_cases:
	/* Same hack to make the rule greedy - I lack time do it 100% properly */
	| c = match_case %prec below_PIPE	{ [ c ] }
	| c = match_case PIPE mc = match_cases
										{ c :: mc }

match_case:
	| p = pattern ARROW e = expr		{ (p, e) }

/*
**	Binding patterns for use by let expressions and function definitions
*/

pattern:
	| ap = atomic_pattern				{ ap }
	/* ADT constructors */
	| c = CTOR ap = atomic_pattern		{ P_Ctor (c, ap) }
	/* Tuples - same technique as the tuple expressions above */
	| pcl = pattern_comma_list
	  %prec below_COMMA					{ P_Tuple (List.rev pcl) }
	/* List constructors, which is a binary operator */
	| p = pattern CONS q = pattern		{ P_Ctor ("Cons", P_Tuple [p;q]) }

atomic_pattern:
	/* Variable names, wildcard, and literal values */
	| n = NAME							{ P_Name n }
	| UND								{ P_Wildcard }
	| i = INT							{ P_Int i }
	| b = BOOL							{ P_Bool b }
	| LPAR RPAR							{ P_Unit }
	/* Parentheses may also appear in patterns */
	| LPAR p = pattern RPAR				{ p }
	/* List patterns */
	| LBRACK p = pattern_list RBRACK	{ p }

pattern_list:
	| /* empty */
		{ P_Ctor ("Empty", P_Unit) }
	| p = pattern
		{ let last = P_Ctor ("Empty", P_Unit) in
		  P_Ctor ("Cons", P_Tuple [p; last]) }
	| p = pattern SEMI pl = pattern_list
		{ P_Ctor ("Cons", P_Tuple [p; pl]) }

pattern_comma_list:
	| p = pattern COMMA q = pattern		{ [q; p] }
	| pcl = pattern_comma_list COMMA r = pattern
										{ r :: pcl }
