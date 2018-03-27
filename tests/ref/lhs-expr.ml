(* Left-hand side of ":=" may be any expression *)
let s = 2 in (ref s) := 4; prInt s
