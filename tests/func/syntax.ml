(* Use different syntax for function definitions *)
let f a b c = a + b + c in
let g = fun x -> fun y -> fun z -> x * y * z in
prInt ((fun a -> f a a a + g a a a) 2)
