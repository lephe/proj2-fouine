(*
**	Machine - A stack-based machine as a compilation target
*)

open Types
open Exceptions
open Pattern
open Memory

open Repr
open Source

(* recursify [machine_instr list -> string -> char list -> machine_instr list]
   Makes the code of a function recursive. The location of the suitable close
   instruction depends on the type of transformation. *)
let recursify code n transf =
	let close instr = match instr with
	| M_Close (None, p, addr) -> M_Close (Some n, p, addr)
	| _ -> raise (InternalError "Trying to build a recursive value") in

	match (transf, code) with
	| ([],			[i])				-> [close i]
	| (['R'],		i :: j :: k)		-> i :: close j :: k
	| (['E'],		i :: j :: k)		-> i :: close j :: k
	| (['R'; 'E'],	i :: j :: k :: l)	-> i :: j :: close k :: l
	| (['E'; 'R'], _) -> raise (InternalError "Machine -RE: TODO x_x")
	| _  -> raise (InternalError "Trying to build a recursive value")

(* machine_eval [private]
   [machine_program -> int -> machine_stack -> machine_env
	 -> machine_state list -> unit]
   Evaluates a machine program using the provided environment and stack. The
   machine state lists represents the try stack. *)
let rec machine_eval prog pc stack env states =

	(* Shorthand when not changing the saved states *)
	let rec eval pc stack env =
		machine_eval prog pc stack env states in

	(* Pop n elements from a list *)
	let pop l n =
		let rec pop_one acc lst n =
			if n = 0 then (List.rev acc, lst) else match lst with
			| x :: tl -> pop_one (x :: acc) tl (n - 1)
			| [] -> raise (InternalError "Machine stack/env ran empty") in
		pop_one [] l n in

	(* Special functions to pop 1 or 2 elements. I return tuples to avoid the
	   non-exhaustive pattern matchings like ([a; b], tail) *)
	let pop1 l = match pop l 1 with
		| ([x], tl) -> (x, tl)
		| _ -> raise (InternalError "machine_eval: pop1: cannot happen (!)") in
	let pop2 l = match pop l 2 with
		| ([x; y], tl) -> (x, y, tl)
		| _ -> raise (InternalError "machine_eval: pop2: cannot happen (!)") in

	(* Same as pop, but also extracts integers *)
	let pop1_arith l = let (x, l) = pop1 l in match x with
		| V_Int a -> (a, l)
		| _ -> raise (InternalError "Machine type error, expected int") in
	let pop2_arith l = let (x, y, l) = pop2 l in match x, y with
		| (V_Int a, V_Int b) -> (a, b, l)
		| _ -> raise (InternalError "Machine type error, expected int") in

	(* arith [int -> int -> int]
	   Automated binary arithmetic operations *)
	let arith f =
		let (a, b, stack) = pop2_arith stack in
		eval (pc + 1) (V_Int (f a b) :: stack) env in

	(* comp [int -> int -> bool]
	   Automated binary comparisons on integers *)
	let comp f =
		let (a, b, stack) = pop2_arith stack in
		eval (pc + 1) (V_Bool (f a b) :: stack) env in

	(* End of program occurs when PC goes out of bounds *)
	if pc >= Array.length prog then () else match prog.(pc) with

	(* Manipulation of values *)
	| M_Push v ->
		eval (pc + 1) (v :: stack) env

	| M_Tuple n ->
		let (values, stack) = pop stack n in
		let tuple = V_Tuple (List.rev values) in
		eval (pc + 1) (tuple :: stack) env

	| M_Ctor name ->
		let (value, stack) = pop1 stack in
		eval (pc + 1) (V_Ctor (name, value) :: stack) env

	| M_Ref ->
		let (value, stack) = pop1 stack in
		eval (pc + 1) (V_Ref (memory_store value) :: stack) env

	(* Functional traits *)

	| M_Let p ->
		let (v, stack') = pop1 stack in
		let bindings = pattern_unify p v in
		eval (pc + 1) stack' (bindings @ env)

	| M_Match p ->
		(* Try to perform the binding:
		   - If it works, remove v from the stack, bind it and push "false" to
			 the stack
		   - Otherwise, push back v to the stack, don't bind anything and push
			 "true" to the stack *)
		let (v, stack) = pop1 stack in
		let (bindings, ret) = try (pattern_unify p v, [V_Bool false]) with
		| MatchError (_, _, _) -> ([], [V_Bool true; v]) in
		eval (pc + 1) (ret @ stack) (bindings @ env)

	| M_EndLet p ->
		let n = StringSet.cardinal (pattern_free p) in
		let (_, env) = pop env n in
		eval (pc + 1) stack env

	| M_Access n ->
		let value = try List.assoc n env with
		| Not_found -> raise (InternalError ("Machine name unknown: " ^ n)) in
		eval (pc + 1) (value :: stack) env

	| M_Close (recursion, n, addr) ->
		begin let closure = match recursion with
		| None -> V_MachineClosure (n, env, addr)
		| Some name ->
			let rec clos = V_MachineClosure (n, (name, clos) :: env, addr) in
			clos in
		eval (pc + 1) (closure :: stack) env
		end

	| M_Apply ->
		begin let (vfun, varg, stack) = pop2 stack in match vfun with
		| V_MachineClosure (p, closure_env, addr) ->
			let frame = V_MachineFrame (env, pc + 1) in
			let bindings = pattern_unify p varg in
			eval addr (frame :: stack) (bindings @ closure_env)
		| V_MachineBuiltin f ->
			f prog pc stack env states varg
		| _ -> print_string (repr_value vfun true); raise (InternalError "Cannot call non-function")
		end

	(* Imperative traits *)

	| M_Jump diff ->
		eval (pc + 1 + diff) stack env

	| M_JumpIf diff ->
		begin let (vbool, stack) = pop1 stack in match vbool with
		| V_Bool b -> eval (if b then pc + 1 + diff else pc + 1) stack env
		| _ -> raise (InternalError "Wrong data type for conditional jump")
		end

	| M_Ret ->
		begin let (vret, vframe, stack) = pop2 stack in match vframe with
		| V_MachineFrame (env, addr) -> eval addr (vret :: stack) env
		| _ -> raise (InternalError "Corrupted stack on function return")
		end

	(* Long jumps *)

	| M_Setjmp diff ->
		let state = (pc + 1 + diff, stack, env) in
		machine_eval prog (pc + 1) stack env (state :: states)

	| M_Longjmp ->
		(* Pass the value at top of stack *)
		let (value, _) = pop1 stack in

		let ((target, stack, env), states) = match states with
		| s :: tl -> (s, tl)
		| [] -> raise (InternalError "Long jump but state stack is empty") in

		machine_eval prog target (value :: stack) env states

	(* Operators *)
	| M_Bang ->
		let (value, stack) = pop1 stack in let data = match value with
		| V_Ref addr -> memory_get addr
		| _ -> raise (InternalError "Trying to bang a non-ref") in
		eval (pc + 1) (data :: stack) env
	| M_Assign ->
		let (e, f, stack) = pop2 stack in
		begin match e with
		| V_Ref addr -> memory_update addr f
		| _ -> raise (InternalError "Trying to assign to a non-ref")
		end;
		eval (pc + 1) (V_Unit :: stack) env

	(* Standard arithmetic *)
	| M_UPlus ->
		let (a, stack) = pop1_arith stack in
		eval (pc + 1) (V_Int (+a) :: stack) env
	| M_UMinus ->
		let (a, stack) = pop1_arith stack in
		eval (pc + 1) (V_Int (-a) :: stack) env
	| M_Add	-> arith ( + )
	| M_Sub	-> arith ( - )
	| M_Mul	-> arith ( * )
	| M_Div	-> arith ( / )

	(* Comparisons between integers *)
	| M_Eq	-> comp  ( = )
	| M_Ne	-> comp ( <> )
	| M_Gt	-> comp  ( > )
	| M_Ge	-> comp ( >= )
	| M_Lt	-> comp  ( < )
	| M_Le	-> comp ( <= )

type builtin = machine_program -> int -> machine_stack -> machine_env
			-> machine_state list -> value -> unit

(* builtin_prInt [builtin] [private] *)
let builtin_prInt prog pc stack env states value = match value with
	| V_Int x ->
		Printf.printf "%d\n" x;
		machine_eval prog (pc + 1) (value :: stack) env states
	| _ -> raise (InternalError "Machine prInt type error")

(* builtin_alloc [builtin] [private] *)
let builtin_alloc prog pc stack env states value = match value with
	| V_Tuple [V_Memory (addr, m); v] ->
		Hashtbl.add m addr v;
		let ret = V_Tuple [V_Ref addr; V_Memory (addr + 1, m)] in
		machine_eval prog (pc + 1) (ret :: stack) env states
	| _ -> raise (InternalError "Machine alloc type error")

(* builtin_read [builtin] [private] *)
let builtin_read prog pc stack env states value = match value with
	| V_Tuple [V_Memory (_, m); V_Ref addr] ->
		let ret = Hashtbl.find m addr in
		machine_eval prog (pc + 1) (ret :: stack) env states
	| _ -> raise (InternalError "Machine read type error")

(* builtin_write [builtin] [private] *)
let builtin_write prog pc stack env states value = match value with
	| V_Tuple [V_Memory (a, m); V_Ref addr; v] ->
		Hashtbl.replace m addr v;
		let ret = V_Memory (a, m) in
		machine_eval prog (pc + 1) (ret :: stack) env states
	| _ -> raise (InternalError "Machine write type error")

(* machine_exec [machine_program -> char list -> value]
   Runs a machine program in an fresh machine. Applied transformations are
   indicated to initialize the environment with appropriate values. *)
let machine_exec prog transf =
	let has_r = List.exists ((=) 'R') transf in
	let builtins =
		(* Under normal conditions: empty environment and only prInt *)
		if not has_r then [
			("prInt",	V_MachineBuiltin builtin_prInt);
		]
		(* When -R is on, provide "s", "alloc", "read" and "write" *)
		else [
			("prInt",	V_MachineBuiltin builtin_prInt);
			("alloc",	V_MachineBuiltin builtin_alloc);
			("read",	V_MachineBuiltin builtin_read);
			("write",	V_MachineBuiltin builtin_write);
			("s",		V_Memory (0, Hashtbl.create 100));
		]
	in machine_eval prog 0 [] builtins []



(*
**	Fouine to stack machine - linker
**
**	The difficult part in compiling the assembler-like language that I'm using
**	is linking, which is assigning locations to functions and computing the
**	destinations of jumps along the way. This design allows the 'close'
**	instruction to refer to the address of a procedure instead of containing
**	actual code.
**
**  This choice reduces the distance between the stack machine and an actual
**	assembler language, making it a bit more interesting. Although there are
**	other unrealistic operations in the language, such as...
**	- Referring to variable names instead of using register/stack allocation;
**	- Matching full patterns in the "let", "match", "endlet" and "close"
**	  instructions instead of breaking them down.
**
**	The following functions are the interface of a linking helper which is
**	designed to create position-independent code for Fouine functions. The
**	compiler will compile a Fouine function using two separate segments: one
**	for the function body, and one for the subfunctions.
**
**		Address,Subfunction			|	Function body
**		0		[Subfunction 1]		|	push 1
**		|		...					|	push 2
**		8		[Subfunction 2]		|	add
**		v		...					|	...
**
**	The compiler generates and returns the function body. It adds instructions
**	to this list until anonymous functions or let-functions are found. Such
**	functions are compiled recursively, but instead of being added to the body,
**	they are pushed to the subfunction segment and a 'close' instruction is
**	added to the body.
**
**	When a subfunction is pushed, the linker returns its absolute address
**	within the subfunction segment. The compiler refers to this address in the
**	generated 'close' instruction.
**
**	When the compiler finishes compiling the program, the linker concatenates
**	the generated body with the subfunction segment. It then updates all the
**	closures to replace the absolute addresses (relative to the subfunction
**	segment) by relative offsets inside the concatenated program.
**
**	When this pass is finished, the linker returns a position-independent
**	machine program whose first instruction is the entry point of the compiled
**	program. Recursion happens naturally as compiling subfunctions might
**	involve sub-subfunctions as well. It is easy to see that the generated
**	subfunction list, when there is no recursion, provides a topological sort
**	of the function dependency graph.
*)

(* A linker context used to link a single function. The body is generated
   "on-the-fly" by the compiler and needs not be stored. *)
type context = {
	(* Subfunction segment. Subfunctions are stored in reverse order. *)
	mutable subs: machine_instr list list;
	(* Size of the segment, equal to List.length (List.concat subs). *)
	mutable size: int;
}

(* context_create [unit -> context]
   Create an empty context to link a new function *)
let context_create () =
	let ctx = { subs = []; size = 0 } in ctx

(* context_push [context -> machine_instr list -> int]
   Pushes a subfunction to the context and returns its absolute address *)
let context_push ctx func =
	let load_address = ctx.size in
	ctx.subs <- func :: ctx.subs;
	ctx.size <- ctx.size + List.length func;
	load_address

(* link [context -> machine_instr list -> machine_instr list]
   Finalizes the linking phase and returns a position-independent program. *)
let link ctx body =
	let offset = List.length body + 1 in

	(* Add an instruction to jump over the subfunctions when the body ends *)
	let jump = if ctx.subs = [] then [] else [M_Jump ctx.size] in

	(* Turn all absolute addresses in relative offsets *)
	let make_relative instr i = match instr with
		| M_Close (n, p, addr) -> M_Close (n, p, addr + offset)
		| x -> x in

	(* A List.map where the function is also provided with the index
	   [('a -> int -> 'b) -> 'a list -> int -> 'b list] *)
	let rec list_map_2 func lst index= match lst with
		| [] -> []
		| x :: tl -> func x index :: list_map_2 func tl (index + 1) in

	(* Concatenate the function body with the subfunctions *)
	let code = body @ jump @ List.concat (List.rev ctx.subs) in
	list_map_2 make_relative code 0



(*
**	Fouine to stack machine - compiler
*)

(* compile [expr -> context -> char list -> machine_instr list] [private]
   Compiles an expression into machine code. Subfunctions are pused to the
   provided linker context; the function body is returned.
   @expr    Expression to compile
   @ctx     Linker context where the program is being compiled
   @transf  Transformations applied on the source code *)
let rec compile expr ctx transf =
	(* Shadow the definition of compile to get rid of the "transf" parameter *)
	let compile e ctx = compile e ctx transf in

	match expr.tree with

	(* Literals and identifiers *)
	| E_Int i 		-> [ M_Push (V_Int i) ]
	| E_Bool b		-> [ M_Push (V_Bool b) ]
	| E_Unit		-> [ M_Push (V_Unit) ]
	| E_Name n		-> [ M_Access n ]

	(* Type constructors *)
	| E_Ctor (c, e) ->
		compile e ctx @ [M_Ctor c]

	(* Pattern matching and try .. with .. *)
	| E_Match (e, cl) ->
		compile e ctx @ compile_cases cl ctx 0 transf
	| E_Try (e, cl) ->
		let code_e	= compile e ctx in
		let cases	= compile_cases cl ctx 1 transf in

		let lcases	= List.length cases in
		let lcode_e	= List.length code_e in

		[M_Setjmp (lcode_e + 1)] @ compile e ctx @ [M_Jump (lcases + 1)]
			@ cases @ [M_Longjmp]

	(* Let bindings *)
	| E_LetVal (p, e, f) ->
		compile e ctx @ [M_Let p] @ compile f ctx @ [M_EndLet p]

	(* Recursive let - create a recursive "close" instruction *)
	| E_LetRec (n, e, f) ->
		let code_e = recursify (compile e ctx) n transf in
		(* Then build the let binding as usual *)
		code_e @ [M_Let (P_Name n)] @ compile f ctx @ [M_EndLet (P_Name n)]

	(* Conditions *)
	| E_If (c, t, f) ->
		let code_t = compile t ctx in
		let code_f = compile f ctx in

		compile c ctx @ [M_JumpIf (List.length code_f + 1)] @ code_f
			@ [M_Jump (List.length code_t)] @ code_t

	(* Functions *)
	| E_Function (p, e) ->
		(* Compile e but don't return it; push it to the context *)
		let addr = context_push ctx (compile e ctx @ [M_Ret]) in
		(* Return a single instruction that creates a closure from this code *)
		[M_Close (None, p, addr)]

	| E_Call (e, f) ->
		compile f ctx @ compile e ctx @ [M_Apply]

	(* References *)
	| E_Ref e ->
		compile e ctx @ [M_Ref]
	| E_Bang e ->
		compile e ctx @ [M_Bang]
	| E_Assign (e, f) ->
		compile f ctx @ compile e ctx @ [M_Assign]

	(* Tuples *)
	| E_Tuple el ->
		let n = List.length el in
		List.concat (List.map (fun e -> compile e ctx) el) @ [M_Tuple n]

	(* Operators *)
	| E_UPlus e				-> compile e ctx @ [M_UPlus]
	| E_UMinus e			-> compile e ctx @ [M_UMinus]
	| E_Plus (e, f)			-> compile f ctx @ compile e ctx @ [M_Add]
	| E_Minus (e, f)		-> compile f ctx @ compile e ctx @ [M_Sub]
	| E_Times (e, f)		-> compile f ctx @ compile e ctx @ [M_Mul]
	| E_Divide (e, f)		-> compile f ctx @ compile e ctx @ [M_Div]
	| E_Equal (e, f)		-> compile f ctx @ compile e ctx @ [M_Eq]
	| E_NotEqual (e, f)		-> compile f ctx @ compile e ctx @ [M_Ne]
	| E_Greater (e, f)		-> compile f ctx @ compile e ctx @ [M_Gt]
	| E_GreaterEqual (e, f)	-> compile f ctx @ compile e ctx @ [M_Ge]
	| E_Lower (e, f)		-> compile f ctx @ compile e ctx @ [M_Lt]
	| E_LowerEqual (e, f)	-> compile f ctx @ compile e ctx @ [M_Le]

(* compile_cases
   [(pattern * expr) list -> context -> int -> char list -> machine_instr list]
   Generates a program that tries to match the provided cases in order.
   @cl         List of cases that are matched
   @ctx        Context where the match or try statement is being compiled
   @jump_base  Number of failure-reporting instructions after the match
   @transf     Transformations applied to source code *)
and compile_cases cl ctx jump_base transf =
		(* Get the raw code for each case *)
		let codes = List.map (fun (p, e) -> (p, compile e ctx transf)) cl in

		(* For each case: try to match. If the binding fails, try the next case
		   by jumping over the code of the case. Otherwise, just execute the
		   case then jump over all the remaining cases. *)

		(* Find out the size of the final jump for each case *)
		let rec get_sizes codes sizes acc = match codes with
		| [] -> sizes
		| ((p, code) :: tl) ->
			get_sizes tl (acc :: sizes) (acc + List.length code + 4) in
		let sizes = get_sizes (List.rev codes) [] jump_base in

		(* Zip the sizes with the cases *)
		let cases = List.combine codes sizes in

		(* Generate the code for each cases: match, check, execute, jump *)
		let make_case ((p, code), size) =
			[M_Match p; M_JumpIf (List.length code + 2)] @ code
				@ [M_EndLet p; M_Jump size] in

		List.concat (List.map make_case cases)

(* machine_compile [program -> char list -> machine_program]
   Compiles a Fouine program into an equivalent stack-machine program *)
let machine_compile prog transf =

	(* Create a context for the whole program *)
	let ctx = context_create () in

	(* compile_statement [statement -> unit]
	   Compiles a statement. The resulting program is pushed to the context. *)
	let compile_statement stmt = match stmt with
		(* Evaluate the instruction and drop the result *)
		| S_Expr (_, e) ->
			compile e ctx transf @ [M_Let P_Wildcard]
		(* Evaluate the expression and match the result *)
		| S_LetVal (_, p, e) ->
			compile e ctx transf @ [M_Let p]
		(* Edit the close instruction inside the code for e *)
		| S_LetRec (_, n, e) ->
			let code_e = recursify (compile e ctx transf) n transf in
			code_e @ [M_Let (P_Name n)]
		(* Assembler does not care *that much* about types *)
		| S_Type (_, _, _) -> [] in

	(* Add the code of "raise" as a subfunction *)
	let addr = context_push ctx
		[M_Access "exn"; M_EndLet (P_Name "exn"); M_Longjmp] in
	(* Declare "raise" at the beginning of the program. The argument binding
	   does not mattern as long as exactly one value is pushed to the env *)
	let code_raise =
		[M_Close (None, P_Name "exn", addr); M_Let (P_Name "raise")] in

	(* Compile all statements in order *)
	let body = code_raise :: List.map compile_statement prog in

	(* Get the resulting program from the linker and return it *)
	Array.of_list (link ctx (List.concat body))
