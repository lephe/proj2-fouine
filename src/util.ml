(*
**	Util - Utility functions, independent from the core interpreter
*)

open Types

(*
	Range objects
	These objects are used to keep track of the file location of the tokens
	and expressions for better error diagnoses.
*)

(* range_str [range -> str]
   Provides a textual representation of the starting point of the range, on the
   form "file:line:column" *)
let range_str ((s, e): range) : string =
	let cnum = s.pos_cnum - s.pos_bol + 1 in
	Printf.sprintf "%s:%02d:%02d" s.pos_fname s.pos_lnum cnum

(* range_merge [range -> range -> range]
   Returns a new range object delimitating the shortest substring that contains
   both (s1, e1) and (s2, e2).
   This function assumes that (s1, e2) and (s2, e2) do not intersect. This is a
   strong condition, but for the parser it's an evidence *)
let range_merge ((s1, e1): range) ((s2, e2): range) : range =
	if s1.pos_cnum < s2.pos_cnum
	then (s1, e2)
	else (s2, e1)

(* range_highlight [string -> range -> unit]
   Highlights a range within a line. This function is outdated... *)
(* TODO: range_higlight: Handle multi-line ranges for file inputs *)
let range_highlight str (_, col, len) =
	(* Adjust the value of column and length *)
	let n = String.length str in
	let c = if col <= n then col else n in
	let l = if c + len <= n then len else n - c in

	(* Print three parts and change color for the middle one *)
	print_string (String.sub str 0 c);
	print_string "\x1b[31;1m";
	print_string (String.sub str c l);
	print_string "\x1b[0m";
	print_string (String.sub str (c + l) (n - c - l))
