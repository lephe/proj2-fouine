(*
**  Typing - Type inference
**  This is a variant of Algorithm W by Luis Damas and Robin Milner.
**
**  Luis Damas, Robin Milner. Principal type-schemes for functional programs
**  https://dl.acm.org/citation.cfm?id=582176
*)

(*
**  Type definitions
*)

exception WError of string

module IntSet = Set.Make(struct
	type t = int
	let compare = Pervasives.compare
end)
module IntMap = Map.Make(struct
	type t = int
	let compare = Pervasives.compare
end)

module StringMap = Map.Make(String)

type exp =
	| E_Int of int
	| E_Bool of bool
	| E_Unit
	| E_Var of string
	| E_Fun of string * exp
	| E_Call of exp * exp
	| E_Pair of exp * exp
	| E_Let of string * exp * exp

(* Monotypes with type variables (called "types" in the original paper) *)
type mtype =
	(* Usual base types *)
	| T_Int
	| T_Bool
	| T_Unit
	(* Type constructors *)
	| T_Fun of mtype * mtype
	| T_Product of int * mtype list
	| T_List of mtype
	(* Named ADTs *)
	| T_ADT of string
	(* Type variables *)
	| T_Var of int

(* Polytypes, aka type schemes in the original paper *)
type ptype = IntSet.t * mtype

(* Type variables, represented here by integers *)
type typevar = int

(* Environments (sets of assumptions). Environments map type variables to
   monotypes in the union-find structure *)
type env = typevar StringMap.t

(*
**  Dummy utilities
*)

(* print_greek [id -> unit]
   Prints a greek letter (0-13) on stdout, assuming utf-8 output *)
let print_greek (id: int) =
	if id > 14 then print_int id else
	let bytes = Bytes.make 2 (Char.chr 0xce) in
	Bytes.set bytes 1 (Char.chr (0xb1 + id));
	print_string (Bytes.to_string bytes)

(* print_mtype [mtype -> unit] *)
let rec print_mtype t = match t with
	| T_Int		-> print_string "int"
	| T_Bool	-> print_string "bool"
	| T_Unit	-> print_string "unit"
	| T_Var v	-> print_greek v
	| T_Fun (t1, t2) ->
		print_string "(";
		print_mtype t1;
		print_string " -> ";
		print_mtype t2;
		print_string ")"
	| T_Product (n, tl) ->
		print_mtype (List.hd tl);
		List.iter (fun x -> print_string " * "; print_mtype x) (List.tl tl)
	| T_List t' ->
		print_mtype t';
		print_string " list"
	| T_ADT s ->
		print_string "s"

(* print_ptype [ptype -> unit] *)
let print_ptype (gen, t) =
	if gen <> IntSet.empty then begin
		print_string "∀";
		IntSet.iter (fun v -> print_string " "; print_greek v) gen;
		print_string ". "
	end;
	print_mtype t

(* print_exp [exp -> unit] *)
let rec print_exp e = match e with
	| E_Int i	-> print_int i
	| E_Bool b	-> print_string (if b then "true" else "false")
	| E_Unit	-> print_string "()"
	| E_Var n	-> print_string n
	| E_Fun (n, f) ->
		print_string "(fun ";
		print_string n;
		print_string " -> ";
		print_exp f;
		print_string ")"
	| E_Call (f, g) ->
		print_string "(";
		print_exp f;
		print_string " ";
		print_exp g;
		print_string ")"
	| E_Pair (f, g) ->
		print_string "(";
		print_exp f;
		print_string ", ";
		print_exp g;
		print_string ")"
	| E_Let (n, f, g) ->
		print_string ("(let " ^ n ^ " = ");
		print_exp f;
		print_string " in ";
		print_exp g;
		print_string ")"

(* print_env [env -> unit] *)
let print_env env =
	StringMap.iter (fun key u ->
		print_string (key ^ " : ");
		print_ptype u;
		print_newline ()
	) env

(*
**  An Union-Find structure to map classes of type variables to monotypes
**  TODO: Add link-by-rank on top of path compression
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
   Returns the monotype associated with a type variable, or itself if it is
   still unresolved *)
let rec uf_find var = match !uf_data.(var) with
	| UF_Null -> T_Var var
	| UF_Type t -> t
	| UF_Link v -> let t = uf_find v in !uf_data.(var) <- UF_Type t; t

(* uf_set [typevar -> mtype -> unit]
   Directly assign a monotype to a type variable *)
let uf_set var t =
	!uf_data.(var) <- UF_Type t

(* uf_union [typevar -> typevar -> unit]
   Perform the union of two type variables' classes. In a typical usage one of
   them at least should be unresolved *)
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
**  Algorithm W
*)

(* The fixed-point combinator *)
let fix = IntSet.of_list [0], T_Fun (T_Fun (T_Var 0, T_Var 0), T_Var 0)

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
(*	print_string "--- Unifying ";
	print_mtype t;
	print_string " and ";
	print_mtype u;
	print_string " ---\n"; *)
	match t, u with
	| T_Int, T_Int
	| T_Bool, T_Bool
	| T_Unit, T_Unit
		-> ()
	| T_Fun (t1, t2), T_Fun (u1, u2)
		-> unify t1 u1; unify t2 u2
	| T_Product (n1, tl), T_Product (n2, ul) when n1 = n2
		-> List.iter (fun (x, y) -> unify x y) (List.combine tl ul)
	| T_List t', T_List u'
		-> unify t' u'
	| T_ADT n, T_ADT m when n = m
		-> ()
	| T_Var v1, T_Var v2
		-> uf_union v1 v2
	| T_Var v, other | other, T_Var v
		-> uf_set v other
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

(* closure [env -> mtype -> ptype]
   Returns the closure of t in environment env *)
let closure env t =
	let generics = IntSet.diff (ftv_mtype t) (ftv_env env) in
	(generics, t)

let rec w env exp = match exp with
	| E_Int _	-> T_Int
	| E_Bool _	-> T_Bool
	| E_Unit	-> T_Unit
	| E_Var v	->
		begin try let u = StringMap.find v env in instance u with
		| Not_found -> T_Var (typevar_new ())
		end
	| E_Fun (v, f) ->
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
	| E_Pair (f, g) ->
		T_Product (2, [w env f; w env g])
	| E_Let (n, f, g) ->
		let t = w env f in
		let u = closure env t in
		w (StringMap.add n u env) g

(* and w env exp =
	print_string "<<< Inferring type of ";
	print_exp exp;
	print_newline ();
	print_env env;
	let u = wrap env exp in
	print_string ">>> ";
	print_mtype (match u with T_Var v -> uf_find v | _ -> u);
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
**  Test cases
*)

let testcase (env, exp) =
	print_string "\n# \x1b[34m";
	print_exp exp;
	print_string "\x1b[0m\n";

	typevar_source := 0;
	let t0 = w env exp in
	let t = rewrite t0 in
	let u = match exp with
	| E_Fun _ | E_Let (_, E_Fun _, _) -> closure env t
	| _ -> (IntSet.empty, t) in

	print_env env;
	print_string "\x1b[33m";
	print_ptype u;
	print_string "\x1b[0m\n"

let _ =
	List.iter (fun p ->
		try testcase p with
		| WError str -> print_string ("error: " ^ str ^ "\n"))
	[
		(StringMap.empty,
		 E_Int 1);

		(StringMap.empty,
		 E_Fun ("x", E_Var "x"));

		(StringMap.empty,
		 E_Fun ("x", E_Fun ("y", E_Var "x")));

		(StringMap.empty,
		 E_Fun ("f", E_Fun ("x", E_Call (E_Var "f", E_Var "x"))));

		(StringMap.empty,
		 E_Call (E_Fun ("x", E_Var "x"), E_Fun ("x", E_Var "x")));

		(StringMap.singleton "f" (IntSet.empty, T_Fun (T_Int, T_Int)),
		 E_Call (E_Var "f", E_Var "x"));

		(StringMap.empty,
		 E_Fun ("f", E_Pair(
			E_Call (E_Var "f", E_Int 0),
			E_Call (E_Var "f", E_Bool true)
		)));

		(StringMap.empty,
		 E_Let ("f",
			E_Fun ("x", E_Var "x"),
		E_Pair (
			E_Call (E_Var "f", E_Int 0),
			E_Call (E_Var "f", E_Bool true)
		)));
	]
