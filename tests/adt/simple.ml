(* Simple constructors *)
type mylist = MyEmpty of unit | MyCons of int * mylist
let v = MyEmpty 3
let x = MyCons (v, -3);;

let MyCons (MyEmpty a, b) = x in
prInt (a + b)
