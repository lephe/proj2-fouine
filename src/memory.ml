(*
**	Memory - Imperative-style, random-access memory for references
**  TODO: Raise a special exception when invalid memory addresses are used
**	TODO: (this will cause problems since exceptions -> repr -> memory...)
*)

open Types

(* Next free address. This is merely an increasing unique id; there is no
   memory allocator nor any form of garbage collection [private] *)
let address : memory_addr ref = ref 1

(* The memory itself, initialized with a guess of 100 elements [private] *)
let mem : memory = Hashtbl.create 100

(* memory_store [value -> memory_addr]
   Stores a new value to memory, and returns its address *)
let memory_store v =
	let addr = !address in
	Hashtbl.add mem addr v;
	incr address;
	addr

(* memory_get [memory_addr -> value]
   Returns the value stored at the provided address. The address must have been
   obtained through a valid reference *)
let memory_get addr =
	Hashtbl.find mem addr

(* memory_update [memory_addr -> value -> unit]
   Updates a referenced object with a new value. The address must have been
   obtained through a valid reference *)
let memory_update addr value =
	Hashtbl.replace mem addr value
