(*
**	Range - Sections of the source code, for error diagnostics
*)

open Types
open Lexing

(* [private] *)
let pos_empty : Lexing.position = {
	pos_fname	= "";
	pos_lnum	= 0;
	pos_bol		= 0;
	pos_cnum	= 0;
}
(* A default value for the range type *)
let range_empty : range = (pos_empty, pos_empty)

(* range_merge [range -> range -> range] *)
let range_merge (s1, e1) (s2, e2) : range =
	if s1.pos_cnum < s2.pos_cnum
	then (s1, e2)
	else (s2, e1)

(* range_highlight [range -> out_channel -> unit]
   TODO: Save the source in a byte object in main; don't reopen the file *)
let range_highlight range out =
	(* Check that the source is a file, otherwise do nothing *)
	if (fst range).pos_fname = "" then () else
	let (start, ende) = range in

	(* Open the source and seek to the last-highlighted line *)
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
