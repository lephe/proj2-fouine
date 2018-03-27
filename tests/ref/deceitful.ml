let addr = ref (ref 0);;

let a = ref 42 in
addr := a;;

prInt !(!addr);;
