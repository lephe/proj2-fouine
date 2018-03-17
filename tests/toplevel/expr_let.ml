(* Toplevel alternative with a single let..in expression. This should be
   seen as a single-expression toplevel by the parser, as in "expr.ml" *)
let x = 3 in
let y = 2 in
x * y + 1
