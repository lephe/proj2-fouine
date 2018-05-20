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
open Repr
open Source
open Exceptions
open Pattern

exception WError of string				(* Generic error *)
exception WUnify of mtype * mtype		(* Cannot unify some types *)

(* Environments (sets of assumptions). Environments map variables names to
   their type variables in the union-find structure. *)
type typeenv = typevar StringMap.t

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

(* Use a hash table with initial size 1024 to store the structure *)
type uf = (int, uf_el) Hashtbl.t
let uf_data = Hashtbl.create 1024

(* uf_register [typevar -> unit] [private]
   Registers the provided type variable in the structure *)
let uf_register var =
	Hashtbl.add uf_data var UF_Null

(* uf_find [typevar -> mtype] [private]
   Returns the monotype associated with a type variable, or the type variable
   itself if it is still "unresolved" *)
let rec uf_find var = match Hashtbl.find uf_data var with
	(* Unresolved variables only link to themselves *)
	| UF_Null -> T_Var var
	(* Follow the link to apply path compression *)
	| UF_Link v ->
		let t = uf_find v in
		Hashtbl.replace uf_data var (UF_Type t); t
	(* Recursively resolve type variables in composite types *)
	| UF_Type t -> uf_resolve t

(* uf_resolve [mtype -> mtype] [private]
   Recursively resolves type variables *)
and uf_resolve t = match t with
	| T_Var v			-> uf_find v
	| T_Fun (t1, t2)	-> T_Fun (uf_resolve t1, uf_resolve t2)
	| T_Product (n, tl)	-> T_Product (n, List.map uf_resolve tl)
	| T_List t'			-> T_List (uf_resolve t')
	| T_Ref t'			-> T_Ref (uf_resolve t')
	| _					-> t

(* uf_assign [typevar -> mtype -> unit] [private]
   Directly assign a monotype to a type variable *)
let uf_assign var t =
	Hashtbl.replace uf_data var (UF_Type t)

(* uf_union [typevar -> typevar -> unit] [private]
   Perform the union of two type variables' classes. In a typical use, one of
   them (at least) will be "unresolved" *)
let uf_union v1 v2 =
	let x, y = if Hashtbl.find uf_data v2 = UF_Null then v2, v1 else v1, v2 in
	Hashtbl.replace uf_data x (UF_Link y)

(* typevar_new [unit -> typevar] [private]
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
(*let fix = IntSet.of_list [0; 1], T_Fun (T_Fun (T_Var 0, T_Var 1), T_Var 0)*)

(* ftv_mtype [mptype -> IntSet.t] [private]
   Returns the set of free type variables of a monotype *)
let rec ftv_mtype t = match t with
	| T_Int | T_Bool | T_Unit | T_ADT _
		-> IntSet.empty
	| T_List t' | T_Ref t'
		-> ftv_mtype t'
	| T_Fun (t1, t2)
		-> IntSet.union (ftv_mtype t1) (ftv_mtype t2)
	| T_Product (_, tl)
		-> List.fold_left IntSet.union IntSet.empty (List.map ftv_mtype tl)
	| T_Var v
		-> IntSet.singleton v

(* ftv_ptype [ptype -> IntSet.t] [private]
   Returns the set of free type variables of a polytype *)
and ftv_ptype (gen, u) =
	IntSet.diff (ftv_mtype u) gen

(* ftv_env [env -> IntSet.t] [private]
   Returns the set of free type variables of an environment *)
and ftv_env env =
	let update key subset set = IntSet.union set subset in
	StringMap.fold update (StringMap.map ftv_ptype env) IntSet.empty

(* unify [mtype -> mtype -> unit] [private]
   Unifies t with u, editing the union-find structure as a side-effect to
   reflect the resolution of type variables *)
and unify t u =
(* Printf.printf "--- Unifying %s and %s ---\n"(repr_mtype t)(repr_mtype u); *)
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
	| T_Ref t', T_Ref u'
		-> unify t' u'

	(* Variables : unite if both sides are variables, otherwise resolve *)
	| T_Var v1, T_Var v2
		-> uf_union v1 v2
	| T_Var v, other | other, T_Var v
		-> uf_assign v other

	(* Of course reject everything else *)
	| _ -> raise (WUnify (t, u))

(* instantiate [ptype -> mtype] [private]
   Returns a new instance of a polytype with fresh type variables *)
and instantiate (gen, t) =
	(* Create a new type variable for each universally-quantified variable *)
	let map = IntSet.fold (fun v map -> IntMap.add v (typevar_new ()) map)
		gen IntMap.empty in

	let rec instantiate t = match t with
		| T_Int
		| T_Bool
		| T_Unit
		| T_ADT _
			-> t
		| T_Fun (t1, t2)
			-> T_Fun (instantiate t1, instantiate t2)
		| T_Product (n, tl)
			-> T_Product (n, List.map instantiate tl)
		| T_List t1
			-> T_List (instantiate t1)
		| T_Ref t1
			-> T_Ref (instantiate t1)
		| T_Var v
			-> T_Var (if IntMap.mem v map then IntMap.find v map else v)
	in instantiate t

(* generalize [env -> mtype -> ptype] [private]
   Return the most generalized polytype of t in environment env *)
and generalize env t =
	let generics = IntSet.diff (ftv_mtype t) (ftv_env env.types) in
	(generics, t)

and w env exp = match exp.tree with

	(* Base cases *)
	| E_Int _	-> T_Int
	| E_Bool _	-> T_Bool
	| E_Unit	-> T_Unit

	(* Instantiate names when they are requested *)
	| E_Name v ->
		begin try let u = StringMap.find v env.types in instantiate u with
		| Not_found -> T_Var (typevar_new ())
		end

	(* For constructors, unify against the declared type *)
	| E_Ctor (n, e) ->
		let (name, mtype) = try StringMap.find n env.adts with
		| Not_found -> raise (WError "undeclared constructor") in
		(* Unify with the constructor's type, if it works, return the ADT *)
		let t = w env e in
		unify (uf_resolve t) mtype;
		T_ADT name

	| E_Match (e, cl)
	| E_Try (e, cl) ->
		let t = w env e in
		let rettype = T_Var (typevar_new ()) in
		let do_one_case (p, e) =
			let bindings = type_breakdown (IntSet.empty, t) p env in
			let bind map (n, t) = StringMap.add n t map in
			let env' = {
				env with types = List.fold_left bind env.types bindings } in
			let u = w env' e in
			unify (uf_resolve rettype) (uf_resolve u) in
		List.iter do_one_case cl;
		rettype

	(* Implement let-polymrphism by generalizing after "let" *)
	| E_LetVal (p, f, g) ->
		let t = w env f in
		let u = generalize env t in
		(* Break down the inferred type according to the pattern *)
		let bindings = type_breakdown u p env in
		let bind map (n, t) = StringMap.add n t map in
		w { env with types = List.fold_left bind env.types bindings } g

	(* Recursion: create a type variable for the recursive name, infer the
	   type of the function, and unify it with the created variable *)
	| E_LetRec (n, f, g) ->
		let t = T_Var (typevar_new ()) in
		let u = w {
			env with types = StringMap.add n (IntSet.empty, t) env.types } f in
		let t = uf_resolve t in
		unify (uf_resolve t) (uf_resolve u);
		let v = generalize env (uf_resolve t) in
		w { env with types = StringMap.add n v env.types } g

	(* Conditions are well-typed if both branches can be unified *)
	| E_If (c, t, f) ->
		let cond	= w env c in
		let btrue	= w env t in
		let bfalse	= w env f in
		(* Make sure condition is of type bool *)
		unify (uf_resolve cond) T_Bool;
		(* Make sure arguments are of the same type *)
		unify (uf_resolve btrue) (uf_resolve bfalse);
		(* Return the type of any of the branches *)
		btrue

	(* Create variable for args and ret, infer types, return arg -> ret *)
	| E_Function (p, f) ->
		(* Create a type variable for each bound variable *)
		let fv = pattern_free p in
		let create n map = StringMap.add n (T_Var (typevar_new ())) map in
		let args = StringSet.fold create fv StringMap.empty in
		(* Extend the typing environment and infer the type of the body *)
		let args_poly = StringMap.map (fun t -> (IntSet.empty, t)) args in
		let env' = { env with types =
			StringMap.union (fun m x y -> Some y) env.types args_poly } in
		let rettype = w env' f in
		(* Build up the type associated with the pattern *)
		let argtype = type_buildup p args env in
		T_Fun (argtype, rettype)

	(* Unify fun_type with arg_type -> any. We resolve t0 and t1 just before
	   unifying because type variables might be fixed at any time *)
	| E_Call (f, a) ->
		let t1 = w env a in
		let t0 = w env f in
		let rettype = T_Var (typevar_new ()) in
		unify (uf_resolve t0) (T_Fun ((uf_resolve t1), rettype));
		rettype

	(* References can be built from any type *)
	| E_Ref e ->
		T_Ref (uf_resolve (w env e))
	(* The return type of (!) is the type of the input reference *)
	| E_Bang e ->
		let arg = w env e in
		let ret = T_Var (typevar_new ()) in
		unify (uf_resolve arg) (T_Ref ret);
		ret
	(* We can assign to any reference, and it yields unit *)
	| E_Assign (e, f) ->
		(* We need only make sure the sub-expression has a type *)
		let _ = w env f in
		let addr = w env e in
		unify (uf_resolve addr) (T_Ref (T_Var (typevar_new ())));
		T_Unit

	(* For tuples, just deduce the type of all elements *)
	| E_Tuple el ->
		T_Product (List.length el, List.map (w env) el)

	(* Arithmetic operations: unify the types of the arguments with T_Int; if
	   it works, return T_Int *)
	| E_UPlus e | E_UMinus e ->
		let arg = w env e in
		unify (uf_resolve arg) T_Int;
		T_Int
	| E_Plus (e, f) | E_Minus (e, f) | E_Times (e, f) | E_Divide (e, f) ->
		let arg2 = w env f in
		let arg1 = w env e in
		unify (uf_resolve arg1) T_Int;
		unify (uf_resolve arg2) T_Int;
		T_Int

	(* Comparisons: unify the arguments with T_Int and return T_Bool *)
	| E_Equal			(e, f)
	| E_NotEqual		(e, f)
	| E_Greater			(e, f)
	| E_GreaterEqual	(e, f)
	| E_Lower			(e, f)
	| E_LowerEqual		(e, f) ->
		let arg2 = w env f in
		let arg1 = w env f in
		unify (uf_resolve arg1) T_Int;
		unify (uf_resolve arg2) T_Int;
		T_Bool

and typevar_rewrite t = match t with
	| T_Var var			-> uf_find var
	| T_Fun (t1, t2)	-> T_Fun (typevar_rewrite t1, typevar_rewrite t2)
	| T_Product (n, tl)	-> T_Product (n, List.map typevar_rewrite tl)
	| T_List t'			-> T_List (typevar_rewrite t')
	| T_Ref t'			-> T_Ref (typevar_rewrite t')
	| _ -> t

(*
**	Free variable generalization and type inference
*)

(* generalizable [expr -> bool] [private]
   Checks whether the given expression is generalizable after type inference *)
and generalizable exp = match exp.tree with
	(* Base types don't have type variables anyway *)
	| E_Int _ | E_Bool _ | E_Unit -> false
	(* Always allow generalizing names - the limiting factor is the env. *)
	| E_Name _ -> true
	(* The constructor's type is always a monotype in this Fouine *)
	| E_Ctor (_, _) -> false
	(* Only if all cases are generalizable *)
	| E_Match (_, cl) | E_Try (_, cl) ->
		List.fold_left (&&) true (List.map (fun (p, e) -> generalizable e) cl)
	(* Let-polymorphism applies if sub-expression is generalizable or a name *)
	| E_LetVal (_, e, _) -> generalizable e
	(* Same as let-val, although normally there will be only functions *)
	| E_LetRec (_, e, _) -> generalizable e
	(* By experience, conditions is generalizable if both branches are *)
	| E_If (_, e, f) -> generalizable e && generalizable f
	(* Functions are always generalized once they are defined *)
	| E_Function (_, _) -> true
	(* Calls are not, as can be seen with (fun x -> x) (fun x -> x) *)
	| E_Call (_, _) -> false
	(* 'ref' and '!' are functions (OCaml-wise), ':=' always returns unit *)
	| E_Ref _ | E_Bang _ | E_Assign (_, _) -> false
	(* By experience, tuple recursively relies on elements *)
	| E_Tuple l -> List.fold_left (&&) true (List.map generalizable l)
	(* All other operators use base types and thus are not generalizable *)
	| E_UPlus _ | E_UMinus _ -> false
	| E_Plus (_, _) | E_Minus (_, _) -> false
	| E_Times (_, _) | E_Divide (_, _) -> false
	| E_Equal (_, _) | E_NotEqual (_, _) -> false
	| E_Greater (_, _) | E_GreaterEqual (_, _) -> false
	| E_Lower (_, _) | E_LowerEqual (_, _) -> false

(* type_infer [expr -> env -> ptype]
   Infers the most general type of the provided expression within the given
   environment *)
and type_infer exp env =
	(* Infer the type of the expression using algorithm W *)
	let inferred = w env exp in
	(* Rewrite the type variables in the result *)
	let t = typevar_rewrite inferred in
	(* Generalize selectively *)
	if generalizable exp then generalize env t else (IntSet.empty, t)

(* type_buildup [pattern -> mtype StringMap.t -> env -> mtype] [private] *)
and type_buildup p types env = match p with
	| P_Name n			-> StringMap.find n types
	| P_Wildcard		-> T_Var (typevar_new ())
	| P_Int i			-> T_Int
	| P_Bool b			-> T_Bool
	| P_Unit			-> T_Unit
	| P_Tuple pl		->
		let n = List.length pl in
		T_Product (n, List.map (fun p -> type_buildup p types env) pl)
	| P_Ctor (n, p')	->
		let typename, mtype = StringMap.find n env.adts in
		let subtype = type_buildup p' types env in
		unify (uf_resolve subtype) mtype;
		T_ADT typename

(* type_breakdown [ptype -> pattern -> (string * ptype) list]
   Breaks down a type to match a pattern, returning the types of all free
   variables of the pattern *)
and type_breakdown ptype pat env =

	(* break [ptype -> pattern -> ptype StringMap.t] *)
	let rec break (gen, mtype) pat = match pat, mtype with
		(* Record the type of a free variable *)
		| (P_Name n, t)
			-> [(n, (gen, t))]
		(* For base types, just check that the pattern matches *)
		| (P_Wildcard, _)
		| (P_Int _, T_Int)
		| (P_Bool _, T_Bool)
		| (P_Unit, T_Unit)
			-> []
		(* Matches recursively and return the union of the recursive maps *)
		| (P_Tuple pl, T_Product (n, tl)) when List.length pl = n ->
			let break_one (p, t) = break (gen, t) p in
			List.concat (List.map break_one (List.combine pl tl))
		(* Check the constructor <> type relation *)
		| (P_Ctor (ctor, p), T_ADT adt) ->
			let (name, mtype) = try StringMap.find ctor env.adts with
			| Not_found -> raise (WError ("unknown constructor " ^ ctor)) in

			(* Then check the provided constructor type *)
			if name <> adt then
				raise (InternalError "failed ctor <> name registration")
			else break (IntSet.empty, mtype) p

		| _ -> raise (WError "cannot break down pattern against type") in

	(* clean [ptype -> ptype]
	   Removes unused type variables from the polymorphic part of a ptype *)
	let clean (name, (gen, t)) =
		(name, (IntSet.inter gen (ftv_mtype t), t)) in

	(* Recursively break down the type, then clean the resulting polytypes *)
	List.map clean (break ptype pat)
