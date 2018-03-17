(* Check that comparisons work well *)
let a = if 1 =  2 then 2      else 1
let b = if 3 <> 4 then 20     else 10
let c = if 5 >  6 then 200    else 100
let d = if 7 >= 2 then 2000   else 1000
let e = if 9 <  4 then 20000  else 10000
let f = if 8 <= 8 then 200000 else 100000;;
prInt (a + b + c + d + e + f)
