(* The prInt function [int -> int] *)
let prInt n =
	print_int n;
	print_newline ();
	n;;

exception E of int;;
