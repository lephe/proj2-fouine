(* Patter matching to destruct Algebraic Data Types *)
type list = Empty | Cons;;

match Cons 4 with
(* There are no constant constructors here, so I must leave a parameter *)
| Empty _ -> prInt 1
| Cons x -> prInt (x - 4)
