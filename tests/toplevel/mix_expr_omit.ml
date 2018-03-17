(* Mixing usual expressions and toplevel-specific semicolon lets is not
   possible. A top-level let..in can only appear just before a semicolon or
   after the last semicolon *)
let x = 3 in let y = 2
let z = 5
let t = 4;;
x * y * z * t
