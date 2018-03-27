(*
**	Util - Utility functions, independent from the core interpreter
*)

(* Open the Lexing module so that range's record type is fully qualified *)
open Types
open Lexing

(*
	Range objects
	These objects are used to keep track of the file location of the tokens
	and expressions for better error diagnoses.
*)

(* [private] *)
let pos_empty : Lexing.position = {
	pos_fname	= "";
	pos_lnum	= 0;
	pos_bol		= 0;
	pos_cnum	= 0;
}
(* A default value for the range type *)
let range_empty = (pos_empty, pos_empty)

(* range_str [range -> str]
   Provides a textual representation of the starting point of the range, on the
   form "file:line:column" *)
let range_str ((s, e): range) : string =
	let cnum = s.pos_cnum - s.pos_bol + 1 in
	let name = if s.pos_fname = "" then "<standard input>" else s.pos_fname in
	Printf.sprintf "%s:%02d:%02d" name s.pos_lnum cnum

(* range_merge [range -> range -> range]
   Returns a new range object delimitating the shortest substring that contains
   both (s1, e1) and (s2, e2).
   This function assumes that (s1, e2) and (s2, e2) do not intersect. This is a
   strong condition, but for the parser it's an evidence *)
let range_merge ((s1, e1): range) ((s2, e2): range) : range =
	if s1.pos_cnum < s2.pos_cnum
	then (s1, e2)
	else (s2, e1)

(* range_highlight [range -> out_channel -> unit]
   Highlights a range within the source. The source script must be in a named
   file, and this function brutally reopens and traverses the file.
   TODO: Save the source in a byte object in main; don't reopen the file *)
let range_highlight (range: range) out =
	(* Check that the source is a file, otherwise do nothing *)
	if (fst range).pos_fname = "" then () else
	let (start, ende) = range in

	(* Open the source and seek to the beginning of the last-higlighted line *)
	let fp = open_in start.pos_fname in
	seek_in fp ende.pos_cnum;

	(* Read a full line to find the byte offset of the first-after-last line *)
	let _ = input_line fp in
	let end_cnum = pos_in fp in

	(* Now read the full highlighted chunk and close the file *)
	seek_in fp start.pos_bol;
	let chunk = Bytes.create (end_cnum - start.pos_bol) in
	really_input fp chunk 0 (end_cnum - start.pos_bol);
	close_in fp;

	(* Three sections: beginning of first line, highlight, end of last line *)
	let l1 = start.pos_cnum - start.pos_bol
	and l2 = ende.pos_cnum - start.pos_cnum
	and l3 = end_cnum - ende.pos_cnum in

	(* Output the sections, changing color for the middle part *)
	output out chunk 0 l1;
	output_string out "\x1b[31;1m";
	output out chunk l1 l2;
	output_string out "\x1b[0m";
	output out chunk (l1 + l2) l3;
