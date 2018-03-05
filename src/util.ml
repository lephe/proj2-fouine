(*
**	Util - Utility functions, independent from the core interpreter
*)

(*
	Range objects
	These objects are used to keep track of the file location of the tokens
	and expressions for better error diagnoses.
*)

(* range - pairs of Lexing.position to delimitate objects *)
type range = Lexing.position * Lexing.position

(* range_str - Get a textual representation on the form "line:column"
   @arg [range]
   @ret [string] *)
let range_str ((s, e): range) : string =
	let cnum = s.pos_cnum - s.pos_bol + 1 in
	Printf.sprintf "%s:%02d:%02d" s.pos_fname s.pos_lnum cnum

(* range_highlight - Print a line and highlight a range
   This function assumes the "str" correspond to the source line referenced in
   the range object, and highlights the corresponding columns. Bound checking
   is performed to gracefully handle errors.
   @arg [string]
   @arg [range] *)
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

(* range_merge - Merge two ranges into one range object that covers the two
   sections *)
let range_merge ((s1, e1): range) ((s2, e2): range) : range =
	(s1, e2)
