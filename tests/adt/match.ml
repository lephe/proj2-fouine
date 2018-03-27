(* Patter matching to destruct Algebraic Data Types *)
type mylist = MyEmpty | MyCons;;

match MyCons 4 with
(* There are no constant constructors here, so I must leave a parameter *)
| MyEmpty _ -> prInt 1
| MyCons x -> prInt (x - 4)
