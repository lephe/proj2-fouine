(*
**	Range - Sections of the source code, for error diagnostics
*)

open Types

(* A default value for the range type, when information is not available *)
val range_empty : range

(* range_merge
   Returns the smallest range object that contains both the arguments as
   substrings. This function assumes that the arguments do not intersect (an
   easy condition for the parser) *)
val range_merge : range -> range -> range

(* range_highlight
   Highlights a range within the source. The source script must be in a named
   file, and this function brutally reopens and traverses the file. *)
val range_highlight : range -> out_channel -> unit
