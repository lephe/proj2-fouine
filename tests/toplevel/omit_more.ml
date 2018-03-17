(* Omitting the last semicolon before the final expression induces strange
   behavior; in this case p is the result f a function call *)
let f x = x
let z = 25;;
let p = f
prInt (2 - (z + 4))
