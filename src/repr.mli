(*
**	Repr - String representation of objects
*)

open Types

(* All the functions of this module return strings. Some take additional
   parameters; such parameters come after the represented object *)

(* repr_expr
   Produces the syntax tree of an expression, indenting each line with the
   requested amount of spaces. Has a final newline *)
val repr_expr : expr -> int -> string

(* repr_memory_addr
   Human-readable form (this is just an integer anyway) *)
val repr_memory_addr : memory_addr -> string

(* repr_pattern
   Returns a human and OCaml-readable representation of a pattern *)
val repr_pattern : pattern -> string

(* repr_range
   Returns a string on the form "file:line:column" that pinpoints the starting
   position of the range *)
val repr_range : range -> string

(* repr_value
   Builds a printable (not unambiguous) representation of a value. If the
   verbose flag is on and the given value is a closure, this function produces
   a multi-line string with the contents of the closure *)
val repr_value : value -> bool -> string

(* repr_statement
   Produces a syntax tree for a statement, very much like repr_expr *)
val repr_statement : statement -> string

(* repr_program
   Returns a printable version of a statement list *)
val repr_program : program -> string
