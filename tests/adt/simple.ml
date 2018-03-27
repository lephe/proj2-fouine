type list = Empty | Cons
let v = Empty 3
let x = Cons (v, -3);;

let Cons (Empty a, b) = x in
prInt (a + b)
