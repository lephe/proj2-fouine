(* The environment in which the exception handler is executed must be that of
   the try statement and not that of the raise call! *)
let y = 0;;
try let y = 2 in raise 0 with
| _ -> prInt y
