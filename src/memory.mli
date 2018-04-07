(*
**	Memory - Imperative-style, random-access memory for references
*)

open Types

(* memory_store
   Stores a new value to memory, and returns its address *)
val memory_store : value -> memory_addr

(* memory_get
   Returns the value stored at the provided address. The address must have been
   obtained through a valid reference *)
val memory_get : memory_addr -> value

(* memory_update
   Updates a referenced object with a new value. The address must have been
   obtained through a valid reference *)
val memory_update : memory_addr -> value -> unit
