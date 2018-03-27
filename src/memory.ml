(*
**	Memory - An imperative-style random-access memory
*)

open Types
open Exceptions

(* Next free address. This is merely an increasing unique id; there is no
   memory allocator nor any form of garbage collection [private] *)
let address : memory_addr ref = ref 1

(* The memory itself, initialized with a guess of 100 elements [private] *)
let mem : memory = Hashtbl.create 100

(* memory_addr_str [memory_addr -> string]
   Provides a textual, unambiguous representation of an address *)
let memory_addr_str = string_of_int

(* memory_create [value -> memory_addr]
   Stores a new value to memory, and returns its address *)
let memory_create v =
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
