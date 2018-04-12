(*
**	Typing - Type inference
**	This is a variant of Algorithm W by Damas and Milner:
**
**	Luis Damas, Robin Milner. Principal type-schemes for functional programs
**	https://dl.acm.org/citation.cfm?id=582176
**
**  See also https://en.wikipedia.org/wiki/Hindley–Milner_type_system.
*)

open Types
open Source

exception WError of string

(* Environments (sets of assumptions). Environments map type variables to
   monotypes in the union-find structure *)
type typeenv = typevar StringMap.t

(*
**	Dummy utilities (these will disappear when I merge the module)
*)

(* repr_mtype [mtype -> string] *)
let rec repr_mtype t = match t with
	| T_Int		-> "int"
	| T_Bool	-> "bool"
	| T_Unit	-> "unit"
	| T_ADT s	-> s
	| T_Var v
		-> Printf.sprintf "%c" (Char.chr (v + 97))
	| T_Fun (t1, t2)
		-> Printf.sprintf "%s -> %s" (repr_mtype t1) (repr_mtype t2)
	| T_Product (_, tl)
		-> String.concat " * " (List.map repr_mtype tl)
	| T_List t'
		-> repr_mtype t' ^ " list"

(* repr_ptype [ptype -> string] *)
let repr_ptype (gen, t) =
	(if gen <> IntSet.empty then
		let varname v acc = acc ^ Printf.sprintf " %c" (Char.chr (v + 97)) in
		IntSet.fold varname gen "∀" ^ ". "
	else "")
	^ repr_mtype t

(* print_env [env -> unit] *)
let print_env env =
	StringMap.iter (fun key u ->
		print_string (key ^ " : ");
		print_string (repr_ptype u);
		print_newline ()
	) env

(*
**	An Union-Find structure to map classes of type variables to monotypes
**	TODO: Add link-by-rank on top of path compression
*)

(* In this implementation:
   - Currently unresolved type variables link nowhere (this includes variables
     that will eventually be declared polymorphic and generalized)
   - Type variable representatives link to their monotype
   - Other type variables link to their representative *)
type uf_el =
	| UF_Null
	| UF_Type of mtype
	| UF_Link of typevar

(* Use an array with initial size 32 to store the Union-FinD structure *)
type uf = uf_el array
let uf_data = ref (Array.make 32 UF_Null)

(* uf_register [typevar]
   Registers the provided type variable in the structure *)
let uf_register var =
	let n = Array.length !uf_data in
	(* If the array is too short, extend it. At least double its size *)
	if var >= n then begin
		let newsize = max (var + 1) (2 * n) in
		let arr = Array.make newsize UF_Null in
		Array.blit !uf_data 0 arr 0 n;
		uf_data := arr;
	end;
	!uf_data.(var) <- UF_Null

(* uf_find [typevar -> mtype]
   Returns the monotype associated with a type variable, or the type variable
   itself if it is still "unresolved" *)
let rec uf_find var = match !uf_data.(var) with
	| UF_Null -> T_Var var
	| UF_Type t -> t
	| UF_Link v -> let t = uf_find v in !uf_data.(var) <- UF_Type t; t

(* uf_resolve [typevar -> mtype -> unit]
   Directly assign a monotype to a type variable *)
let uf_resolve var t =
	!uf_data.(var) <- UF_Type t

(* uf_union [typevar -> typevar -> unit]
   Perform the union of two type variables' classes. In a typical use one of
   them (at least) will be "unresolved" *)
let uf_union v1 v2 =
	let x, y = if !uf_data.(v2) = UF_Null then v2, v1 else v1, v2 in
	!uf_data.(x) <- UF_Link y

(* typevar_new [unit -> typevar]
   Returns a new, unique type variable identifier, and registers it *)
let typevar_source = ref 0
let typevar_new () =
	let var = !typevar_source in
	uf_register var;
	incr typevar_source;
	var

(*
**	Algorithm W
*)

(* The fixed-point combinator - not used yet *)
let fix = IntSet.of_list [0; 1], T_Fun (T_Fun (T_Var 0, T_Var 1), T_Var 0)

(* ftv_mtype [mptype -> IntSet.t]
   Returns the set of free type variables of a monotype *)
let rec ftv_mtype t = match t with
	| T_Int | T_Bool | T_Unit | T_ADT _
		-> IntSet.empty
	| T_List t'
		-> ftv_mtype t'
	| T_Fun (t1, t2)
		-> IntSet.union (ftv_mtype t1) (ftv_mtype t2)
	| T_Product (_, tl)
		-> List.fold_left IntSet.union IntSet.empty (List.map ftv_mtype tl)
	| T_Var v
		-> IntSet.singleton v

(* ftv_ptype [ptype -> IntSet.t]
   Returns the set of free type variables of a polytype *)
let ftv_ptype (gen, u) =
	IntSet.diff (ftv_mtype u) gen

(* ftv_env [env -> IntSet.t]
   Returns the set of free type variables of an environment *)
let ftv_env env =
	let update key subset set = IntSet.union set subset in
	StringMap.fold update (StringMap.map ftv_ptype env) IntSet.empty

(* unify [mtype -> mtype -> unit]
   Unifies t with u, editing the union-find structure as a side-effect to
   reflect the resolution of type variables *)
let rec unify t u =
(*	Printf.printf "--- Unifying %s and %s ---\n" (repr_mtype t) (repr_mtype u);
*)
	match t, u with

	(* Bases case s*)
	| T_Int, T_Int
	| T_Bool, T_Bool
	| T_Unit, T_Unit
		-> ()
	| T_ADT n, T_ADT m when n = m
		-> ()

	(* Recursive cases *)
	| T_Fun (t1, t2), T_Fun (u1, u2)
		-> unify t1 u1; unify t2 u2
	| T_Product (n1, tl), T_Product (n2, ul) when n1 = n2
		-> List.iter (fun (x, y) -> unify x y) (List.combine tl ul)
	| T_List t', T_List u'
		-> unify t' u'

	(* Variables : unite if both sides are variables, otherwise resolve *)
	| T_Var v1, T_Var v2
		-> uf_union v1 v2
	| T_Var v, other | other, T_Var v
		-> uf_resolve v other

	(* Of course reject everything else *)
	| _ -> raise (WError "unify failed")

(* instance [ptype -> mtype]
   Returns a new instance of a polytype with fresh type variables *)
let instance (gen, t) =
	(* Create new type variables for each universally-quantified variable *)
	let map = IntSet.fold (fun v map -> IntMap.add v (typevar_new ()) map)
		gen IntMap.empty in

	let rec sub map t = match t with
		| T_Int
		| T_Bool
		| T_Unit
		| T_ADT _
			-> t
		| T_Fun (t1, t2)
			-> T_Fun (sub map t1, sub map t2)
		| T_Product (n, tl)
			-> T_Product (n, List.map (sub map) tl)
		| T_List t1
			-> T_List (sub map t1)
		| T_Var v
			-> T_Var (if IntMap.mem v map then IntMap.find v map else v)
	in sub map t

(* generalize [env -> mtype -> ptype]
   Return the most generalized polytype of t in environment env *)
let generalize env t =
	let generics = IntSet.diff (ftv_mtype t) (ftv_env env) in
	(generics, t)

let rec w env exp = match exp.tree with
	| E_Int _	-> T_Int
	| E_Bool _	-> T_Bool
	| E_Unit	-> T_Unit
	| E_Name v	->
		begin try let u = StringMap.find v env in instance u with
		| Not_found -> T_Var (typevar_new ())
		end
	(* TODO: Extend functions to more complicated patterns *)
	| E_Function (P_Name v, f) ->
		let argtype = T_Var (typevar_new ()) in
		let env' = StringMap.add v (IntSet.empty, argtype) env in
		let rettype = w env' f in
		T_Fun (argtype, rettype)
	| E_Call (f, a) ->
		let t0 = w env f in
		let t1 = w env a in
		let rettype = T_Var (typevar_new ()) in
		let u0 = match t0 with
		| T_Var v -> uf_find v
		| _ -> t0 in
		let u1 = match t1 with
		| T_Var v -> uf_find v
		| _ -> t1 in
		unify u0 (T_Fun (u1, rettype));
		rettype
	| E_Tuple el ->
		T_Product (List.length el, List.map (w env) el)
	(* TODO: Extend let-value to more complicated patterns *)
	| E_LetVal (P_Name n, f, g) ->
		let t = w env f in
		let u = generalize env t in
		w (StringMap.add n u env) g

(* and w env exp =
	print_string "<<< Inferring type of ";
	print_exp exp;
	print_newline ();
	print_env env;
	let u = wrap env exp in
	print_string ">>> ";
	print_string (repr_mtype (match u with T_Var v -> uf_find v | _ -> u));
	print_newline ();
	u
*)

let rec rewrite t = match t with
	| T_Var var -> uf_find var
	| T_Fun (t1, t2) -> T_Fun (rewrite t1, rewrite t2)
	| T_Product (n, tl) -> T_Product (n, List.map rewrite tl)
	| T_List t' -> T_List (rewrite t')
	| _ -> t

(*
**	Test cases
*)

let testcase (env, exp) =
	print_string "\n# \x1b[34m";
	print_string (source_expr exp 0);
	print_string "\x1b[0m\n";

	typevar_source := 0;
	let t0 = w env exp in
	let t = rewrite t0 in
	let u = match exp.tree with
	(* TODO - Care about this when introducing more complicated patterns *)
	| E_Function _
	| E_LetVal (P_Name _, { tree = E_Function _ }, _) -> generalize env t
	| _ -> (IntSet.empty, t) in

	print_env env;
	print_string "\x1b[33m";
	print_string (repr_ptype u);
	print_string "\x1b[0m\n"

open Range

let r = range_empty
let (%) r t = { range = r; tree = t }

let typing_tests () =
	List.iter (fun p ->
		try testcase p with
		| WError str -> print_string ("error: " ^ str ^ "\n"))
	[
		(StringMap.empty,
		 r% E_Int 1);

		(StringMap.empty,
		 r% E_Function (P_Name "x", r% E_Name "x"));

		(StringMap.empty,
		 r% E_Function (P_Name "x", r% E_Function (P_Name "y", r% E_Name "x"))
		);

		(StringMap.empty,
		 r% E_Function (P_Name "f",
			r% E_Function (P_Name "x",
				r% E_Call (r% E_Name "f", r% E_Name "x")
		)));

		(StringMap.empty,
		 r% E_Call(
			r% E_Function (P_Name "x", r% E_Name "x"),
			r% E_Function (P_Name "x", r% E_Name "x")
		));

		(StringMap.singleton "f" (IntSet.empty, T_Fun (T_Int, T_Int)),
		 r% E_Call (r% E_Name "f", r% E_Name "x"));

		(StringMap.empty,
		 r% E_Function (P_Name "f", r% E_Tuple [
			r% E_Call (r% E_Name "f", r% E_Int 0);
			r% E_Call (r% E_Name "f", r% E_Bool true)
		]));

		(StringMap.empty,
		 r% E_LetVal (P_Name "f",
			r% E_Function (P_Name "x", r% E_Name "x"),
		r% E_Tuple [
			r% E_Call (r% E_Name "f", r% E_Int 0);
			r% E_Call (r% E_Name "f", r% E_Bool true)
		]));
	]
