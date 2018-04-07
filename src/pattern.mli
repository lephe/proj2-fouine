(*
**	Pattern - Pattern-matching expressions for let, match, and functions
*)

open Types

(* pattern_free
   Returns a list of all the names bound by the pattern. This function does not
   indicate whether there are duplicates or not. Only pattern_unify will fail
   in this case *)
val pattern_free : pattern -> StringSet.t

(* pattern_unify
   Implements syntactic unification (actually filtering) to bind a pattern to
   a value, or raise a pattern-matching exception. Returns a list of bindings
   on the form (name, value) on success *)
val pattern_unify : pattern -> value -> (string * value) list

(* pattern_bind
   A higher-level interface to pattern_unif. Matches the given value against
   the pattern, and extends the environment with the resulting bindings *)
val pattern_bind : pattern -> value -> env -> env
