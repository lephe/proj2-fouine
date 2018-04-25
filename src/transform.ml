(*
**	Program transformations that eliminate traits
*)

open Types
open Range
open Exceptions

(* When performing a transformation, there is no source code so no ranges to
   assign. I will use this one instead *)
let r = range_empty

(* list_init - Just List.init without requiring Ocaml 4.06 *)
let rec list_init n f =
	let rec do_one m = if m = n then [] else f m :: do_one (m + 1)
	in do_one 0

(*
**	Variable renaming
*)

(* Rename an expression *)
let rec (!>) e = { e with tree = rename_expr e.tree }

(* rename_pat [pattern -> pattern] [private]
   Prepends a "_" to all names of a pattern *)
and rename_pat pat = match pat with
	(* Terminal cases *)
	| P_Int _		-> pat
	| P_Bool _		-> pat
	| P_Unit		-> pat
	| P_Wildcard	-> P_Wildcard
	(* Rename all names because there is no built-in in the pattern context *)
	| P_Name n		-> P_Name ("_" ^ n)
	(* Recursive cases *)
	| P_Tuple pl	-> P_Tuple (List.map rename_pat pl)
	| P_Ctor (n, p)	-> P_Ctor (n, rename_pat p)

(* rename_expr [expr_tree -> expr_tree] [private]
   Prepends a "_" to all non-built-in names of an expression *)
and rename_expr tree =
	(* case [pattern * expr -> pattern * expr]
	   Renames a match or try case *)
	let case (p, e) = (rename_pat p, !> e) in

	match tree with
	(* TODO - Look up built-in names in the environment instead *)
	| E_Name n ->
		E_Name (if n = "prInt" || n = "raise" || n = "read" || n = "write" ||
			n = "alloc" then n else "_" ^ n)
	(* Terminal cases *)
	| E_Int _					-> tree
	| E_Bool _					-> tree
	| E_Unit					-> tree
	(* A lot of trivial recursion, not even worth mentioning *)
	| E_Ctor		(n, e)		-> E_Ctor			(n, !> e)
	| E_Match		(e, pl)		-> E_Match			(!> e, List.map case pl)
	| E_Try			(e, pl)		-> E_Try			(!> e, List.map case pl)
	| E_LetVal		(p, e, f)	-> E_LetVal			(rename_pat p, !> e, !> f)
	| E_LetRec		(n, e, f)	-> E_LetRec			("_" ^ n, !> e, !> f)
	| E_If			(e, t, f)	-> E_If				(!> e, !> t, !> f)
	| E_Function	(p, e)		-> E_Function		(rename_pat p, !> e)
	| E_Call		(e, f)		-> E_Call			(!> e, !> f)
	| E_Ref			(e)			-> E_Ref			(!> e)
	| E_Bang		(e)			-> E_Bang			(!> e)
	| E_Assign		(e, f)		-> E_Assign			(!> e, !> f)
	| E_Tuple		(el)		-> E_Tuple			(List.map (!>) el)
	| E_UPlus		(e)			-> E_UPlus			(!> e)
	| E_UMinus		(e)			-> E_UMinus			(!> e)
	| E_Plus		(e, f)		-> E_Plus			(!> e, !> f)
	| E_Minus		(e, f)		-> E_Minus			(!> e, !> f)
	| E_Times		(e, f)		-> E_Times			(!> e, !> f)
	| E_Divide		(e, f)		-> E_Divide			(!> e, !> f)
	| E_Equal		(e, f)		-> E_Equal			(!> e, !> f)
	| E_NotEqual	(e, f)		-> E_NotEqual		(!> e, !> f)
	| E_Greater		(e, f)		-> E_Greater		(!> e, !> f)
	| E_GreaterEqual(e, f)		-> E_GreaterEqual	(!> e, !> f)
	| E_Lower		(e, f)		-> E_Lower			(!> e, !> f)
	| E_LowerEqual	(e, f)		-> E_LowerEqual		(!> e, !> f)

(* transform_rename [program -> program]
   Renames all expression variables, except the built-in names, to avoid
   conflicts with names generated by the transformations. This renaming only
   prepends a "_" to each name *)
let transform_rename stmts =
	let rename_stmt stmt = match stmt with
	| S_Expr	(r, e)		-> S_Expr	(r, !> e)
	| S_LetVal	(r, p, e)	-> S_LetVal	(r, rename_pat p, !> e)
	| S_LetRec	(r, n, e)	-> S_LetRec	(r, "_" ^ n, !> e)
	| S_Type	(r, s, cs)	-> S_Type	(r, s, cs)
	in List.map rename_stmt stmts



(*
**	Transformation helpers
**	The following functions construct common subtrees for the imperative and
**	continuation transformations. They serve several purposes:
**	- factor common elements to avoid lengthy trees
**	- hide the "r%" things that make the code less readable
**
**  The following transformations are used (mainly):
**    [terminal] = fun s -> (terminal, s)
**    [fun x -> e] = fun s -> (fun x -> [e], s)
**    [f g] = fun s -> let (vg, s) = [g] s in let (vf, s) = [f] s in vf vg s
**    [let p = e in f ] = let (p, s) = [e] s in [f] s
**    [let rec n = e in f] = let rec n = fst ([e] s) in [f] s
**    [!e] = fun s -> let (ve, s) = [e] s in (read ve s, s)
**    [e := f] = fun s -> let (vf, s) = [f] s in let (ve, s) = [e] s in
**               ((), write s ve vf)
**    [ref e] = fun s -> let (ve, s) = [e] s in alloc s ve
**  For try, exception E x is turned into E (x, s).
*)

(* call [expr_tree list -> expr_tree]
   "call [x; ...]" returns a nested function call of x with arguments taken
   from the list *)
let call exps =
	let rec call_rev exps = match exps with
		| [] | [_]	-> raise (InternalError "call: you must pass arguments")
		| [b; a]	-> E_Call (r% a, r% b)
		| z::y::tl	-> E_Call (r% call_rev (y::tl), r% z) in
	call_rev (List.rev exps)

(* tuple [expr_tree list -> expr_tree]
   "tuple [x; ...]" returns a tuple structure (x, ...) *)
let tuple exps =
	E_Tuple (List.map ((%) r) exps)

(* name [string -> expr_tree] *)
let name s = E_Name s

(* letval [pattern -> expr_tree -> expr_tree -> expr_tree] *)
let letval p e f = E_LetVal (p, r% e, r% f)

(*
**	Elimination of imperative traits
*)

(* This operator now performs imperative elimination. It also adds the usual
   "fun s ->" on the fly *)
let rec (!>) e = E_Function (P_Name "s", r% imperative_expr e.tree)

(* imperative_expr [expr_tree -> expr_tree]
   Removes the imperatives traits of an expression. A lot of this is difficult
   to read, and for the sake of simplicity I have often renamed s' or s'' in s,
   relying on let-shadowing to get the job done. I also simplified the way I
   handle operators by using this trick. *)
and imperative_expr tree =

	(* return [expr_tree -> expr_tree -> expr_tree]
	   From "e" and "s", yields a tree "(e, s)" *)
	let return e s = tuple [e; s] in

	(* qret [expr_tree -> expr_tree]
	   From "e", yields "(e, s)" where "s" is the memory *)
	let qret e = return e (name "s") in

	(* qlet [string -> expr -> expr_tree -> expr_tree]
	   Yields "let (result, s) = [| exp |] s in exp'" *)
	let qlet result exp exp' =
		letval
			(P_Tuple [P_Name result; P_Name "s"])
			(call [!> exp; name "s"])
			exp' in

	(* qrec [estring -> expr -> expr_tree -> expr_tree]
	   Yields "let rec func = fst ([| exp |] s) in exp'" *)
	let qrec func exp exp' =
		E_LetRec (func, r% qlet func exp (name func), r% exp') in

	(* binary [(expr -> expr -> expr_tree) -> expr -> expr -> expr_tree]
	   From a binary node constructor (e, f) -> <a tree> and tree e, f, yields
	   "let (vf, s) = [| f |] s in let (ve, s) = [| e |] s in <the tree>" *)
	let binary ctor e f =
		qlet "vf" f (qlet "ve" e (qret (ctor (r% name "ve") (r%name "vf")))) in

	match tree with

	(* Terminal cases *)
	| E_Int _		-> qret tree
	| E_Bool _		-> qret tree
	| E_Unit		-> qret tree

	(* For built-in functions, create a wrapper on the form:
	     "prInt" -> "fun x s -> (prInt x, s)" *)
	| E_Name n ->
		(* TODO: Use a built-in name API, like looking up the environment *)
		if n = "prInt" then
			qret (E_Function (P_Name "x", r% E_Function (P_Name "s",
				r% qret (call [name n; name "x"]))))
		else if n = "raise" then
			qret (E_Function (P_Name "x", r% E_Function (P_Name "s",
				r% call [name n; tuple [name "x"; name "s"]])))
		else qret tree

	(* Standard lambda calculus *)
	| E_Function (p, e)	-> qret (E_Function (p, r% !> e))
	| E_Call (e, f) ->
		qlet "vf" f (qlet "ve" e (
		call [name "ve"; name "vf"; name "s"]))
	| E_LetVal (p, e, f) ->
		letval
			(P_Tuple [p; P_Name "s"])
			(call [!> e; name "s"])
			(call [!> f; name "s"])
	| E_LetRec (n, e, f) ->
		qrec n e (call [!> f; name "s"])

	(* Imperative traits *)
	| E_Bang e ->
		qlet "ve" e (return
			(call [name "read"; tuple [name "s"; name "ve"]]) (name "s"))
	| E_Assign (e, f) ->
		qlet "ve" e (qlet "vf" f (return E_Unit
		(call [name "write"; tuple [name "s"; name "ve"; name "vf"]])))
	| E_Ref e ->
		qlet "ve" e (call [name "alloc"; tuple [name "s"; name "ve"]])

	(* Other ML features *)
	| E_If (c, t, f) ->
		qlet "vc" c (E_If (r% name "vc",
			r% call [!> t; name "s"],
			r% call [!> f; name "s"]))
	| E_Match (e, cl) ->
		let do_case (p, e) = (p, r% call [!> e; name "s"]) in
		qlet "ve" e (E_Match (r% name "ve", List.map do_case cl))
	| E_Ctor (ctor, e) ->
		qlet "ve" e (qret (E_Ctor (ctor, r% name "ve")))
	| E_Tuple el ->
		(* Give names to the values of the tuple *)
		let n = List.length el in
		let names = list_init n (fun i -> "v" ^ (string_of_int i)) in
		(* Add numbers to elements of the list *)
		let el' = List.combine names el in
		qret (List.fold_right (fun (n, e) r -> qlet n e r) el'
			(tuple (List.map name names)))

	(* For try statements, make raise throw E (_E x, s) instead of _E x *)
	| E_Try (e, cl) ->
		let do_case (p, e) = (
			P_Tuple [p; P_Name "s"],
			r% call [!> e; name "s"]) in
		E_Try (r% call [!> e; name "s"], List.map do_case cl)

	(* Boring operators... *)
	| E_UPlus e  -> qlet "ve" e (qret (E_UPlus  (r% name "ve")))
	| E_UMinus e ->	qlet "ve" e (qret (E_UMinus (r% name "ve")))
	| E_Plus		(e, f) -> binary (fun e f -> E_Plus			(e, f)) e f
	| E_Minus		(e, f) -> binary (fun e f -> E_Minus		(e, f)) e f
	| E_Times		(e, f) -> binary (fun e f -> E_Times		(e, f)) e f
	| E_Divide		(e, f) -> binary (fun e f -> E_Divide		(e, f)) e f
	| E_Equal		(e, f) -> binary (fun e f -> E_Equal		(e, f)) e f
	| E_NotEqual	(e, f) -> binary (fun e f -> E_NotEqual		(e, f)) e f
	| E_Greater		(e, f) -> binary (fun e f -> E_Greater		(e, f)) e f
	| E_GreaterEqual(e, f) -> binary (fun e f -> E_GreaterEqual	(e, f)) e f
	| E_Lower		(e, f) -> binary (fun e f -> E_Lower		(e, f)) e f
	| E_LowerEqual	(e, f) -> binary (fun e f -> E_LowerEqual	(e, f)) e f



(* transform_imperative [program -> program]
   Removes the imperative traits of the given program while preserving its
   semantics *)
let transform_imperative stmts =

	(* A few helpers, again *)
	let call e = call [!> e; name "s"] in
	let _fst e = letval (P_Tuple [P_Name "a"; P_Name "b"]) e (name "a") in

	(* [statement -> statement] *)
	let transform stmt = match stmt with
	(* When expressions are written at the top level, update the memory, "s" *)
	| S_Expr (r, e) ->
		S_LetVal (r, P_Tuple [P_Wildcard; P_Name "s"], r% call e)
	(* Other constructs behave as usual; we also update "s". For S_LetRec there
	   is a call to "fst" to extract the recursive expression *)
	| S_LetVal (r, p, e) ->
		S_LetVal (r, P_Tuple [p; P_Name "s"], r% call e)
	| S_LetRec (r, n, e) ->
		S_LetRec (r, n, r% _fst (call e))
	| S_Type (r, s, cs) ->
		S_Type (r, s, cs)

	in List.map transform (transform_rename stmts)



(*
**	Introduction of continuations
**
**  The following transformations are used (mainly):
**    [terminal] = fun k kE -
**       k terminal
**    [fun x -> e] = fun k kE ->
**       k (fun x -> [e])
**    [f g] = fun k kE ->
**      [g] (fun vg -> [f] (fun vf -> vf vg k kE) kE) kE
**    [let p = e in f] = fun k kE ->
**      [e] (fun p -> [f] k kE) kE
**    [let rec n = e in f] = fun k kE ->
**      let rec n = [e] (fun x -> x) kE in [f] k kE
**    [try e with p1 -> e1 | ... | pn -> en] = fun k kE ->
**      [e] k (fun exc -> match exc with ... | pn -> en | x -> kE x)
**  The let rec case is not really continuation-style since the recursive call
**  is not tail-recursion.
*)

(* Now use this operator to introduce continuations and add the usual
   "fun k ke ->" on the fly *)
let rec (!>) e =
	E_Function (P_Name "k", r% E_Function (P_Name "kE",
		r% continuation_expr e.tree))

(* continuation_expr [expr_tree -> expr_tree -> expr_tree]
   Eliminates exception by introducing continuations *)
and continuation_expr tree =

	(* qret [expr_tree -> expr_tree]
	   "Quick return": calls "k" with the given expression tree *)
	let qret arg = call [name "k"; arg] in

	(* qfwd [expr_tree -> string -> expr_tree -> expr_tree]
	   "Quick forward". "qfwd exp vexp future" yields the tree of
	   "[| exp |] (fun vexp -> future) kE" *)
	let qfwd exp vexp future =
		call [!> exp; E_Function (P_Name vexp, r% future); name "kE"] in

	(* binary [(expr -> expr -> expr_tree) -> expr -> expr -> expr_tree] *)
	let binary ctor e f =
		qfwd f "vf" (qfwd e "ve" (qret (ctor (r% name "ve") (r%name "vf")))) in

	match tree with

	(* Terminal cases *)
	| E_Int _		-> qret tree
	| E_Bool _		-> qret tree
	| E_Unit		-> qret tree

	| E_Name n ->
		if n = "prInt" || n = "read" || n = "write" || n = "alloc"
		then qret (E_Function (P_Name "x", r% E_Function (P_Name "k",
			r% E_Function (P_Name "kE", r% call [name "k";
			call [name n; name "x"]]))))
		else if n = "raise"
		then qret (E_Function (P_Name "x", r% E_Function (P_Name "k",
			r% E_Function (P_Name "kE", r% call [name "kE"; name "x"]))))
		else qret tree

	(* Standard lambda calculus *)
	| E_Function (p, e) -> qret (E_Function (p, r% !> e))
	| E_Call (e, f) ->
		qfwd f "arg" (qfwd e "func"
			(call [name "func"; name "arg"; name "k"; name "kE"]))
	| E_LetVal (p, e, f) ->
		qfwd e "ve" (letval p (name "ve") (call [!> f; name "k"; name "kE"]))
	| E_LetRec (n, e, f) ->
		E_LetRec (n, r% qfwd e "x" (name "x"),
			r% call [!> f; name "k"; name "kE"])

	(* Exceptions *)
	| E_Try (e, cl) ->
		let do_case (p, e) = (p, r% call [!> e; name "k"; name "kE"]) in
		let last_case = (P_Name "exc", r% call [name "kE"; name "exc"]) in
		call [!> e; name "k"; E_Function (P_Name "exc",
			r% E_Match (r% name "exc", List.map do_case cl @ [last_case]))]

	(* Imperative traits *)
	| E_Bang e ->
		qfwd e "ve" (qret (E_Bang (r% name "ve")))
	| E_Assign (e, f) ->
		qfwd f "vf" (qfwd e "ve"
			(qret (E_Assign (r% name "ve", r% name "vf"))))
	| E_Ref e ->
		qfwd e "ve" (qret (E_Ref (r% name "ve")))

	(* Other ML features *)
	| E_If (c, t, f) ->
		qfwd c "vc" (E_If (r% name "vc", r% call [!> t; name "k"; name "kE"],
			r% call [!> f; name "k"; name "kE"]))
	| E_Match (e, cl) ->
		let do_case (p, e) = (p, r% call [!> e; name "k"; name "kE"]) in
		qfwd e "ve" (E_Match (r% name "ve", List.map do_case cl))
	| E_Ctor (ctor, e) ->
		qfwd e "ve" (qret (E_Ctor (ctor, r% name "ve")))
	| E_Tuple l ->
		let rec do_list els acc = match els with
		| [] -> acc
		| (e, n) :: tl -> do_list tl (qfwd e n acc) in
		let names = list_init (List.length l) (fun i -> "x"^string_of_int i) in
		do_list (List.rev (List.combine l names))
			(qret (tuple (List.map name names)))

	(* Uninteresting operators *)
	| E_UPlus e  -> qfwd e "ve" (qret (E_UPlus  (r% name "ve")))
	| E_UMinus e -> qfwd e "ve" (qret (E_UMinus (r% name "ve")))
	| E_Plus		(e, f) -> binary (fun e f -> E_Plus			(e, f)) e f
	| E_Minus		(e, f) -> binary (fun e f -> E_Minus		(e, f)) e f
	| E_Times		(e, f) -> binary (fun e f -> E_Times		(e, f)) e f
	| E_Divide		(e, f) -> binary (fun e f -> E_Divide		(e, f)) e f
	| E_Equal		(e, f) -> binary (fun e f -> E_Equal		(e, f)) e f
	| E_NotEqual	(e, f) -> binary (fun e f -> E_NotEqual		(e, f)) e f
	| E_Greater		(e, f) -> binary (fun e f -> E_Greater		(e, f)) e f
	| E_GreaterEqual(e, f) -> binary (fun e f -> E_GreaterEqual	(e, f)) e f
	| E_Lower		(e, f) -> binary (fun e f -> E_Lower		(e, f)) e f
	| E_LowerEqual	(e, f) -> binary (fun e f -> E_LowerEqual	(e, f)) e f

(* transform_continuation [program -> program]
   Eliminates exceptions by rewriting the program in continuation-style *)
let transform_continuation stmts =

	(* The identity continuation *)
	let k0 = E_Function (P_Name "x", r% name "x") in

	(* [statement -> statement] *)
	let transform stmt = match stmt with
	(* Call bare expressions with the basic continuations *)
	| S_Expr (r, e) -> S_Expr (r, r% call [!> e; k0; k0])
	(* Do not change the semantics of let-val and let-rec; same for types *)
	| S_LetVal (r, p, e) -> S_LetVal (r, p, r% call [!> e; k0; k0])
	| S_LetRec (r, n, e) -> S_LetRec (r, n, r% call [!> e; k0; k0])
	| S_Type (r, s, cts) -> S_Type (r, s, cts)

	in List.map transform (transform_rename stmts)


(*
**	General interface
*)

(* transform [program -> char list -> program]
   Repeatedly transform the program by taking transformation name in the list:
   - 'R' eliminates imperative traits
   - 'E' introduces continuations *)
let rec transform p tr_list = match tr_list with
	| [] -> p
	| 'R' :: tl -> transform (transform_imperative p) tl
	| 'E' :: tl -> transform (transform_continuation p) tl
	|  _  :: tl -> transform p tl