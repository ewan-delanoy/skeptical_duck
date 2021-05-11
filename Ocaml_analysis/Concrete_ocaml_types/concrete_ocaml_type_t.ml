(* 

#use"Ocaml_analysis/Concrete_ocaml_types/concrete_ocaml_type_t.ml";;

You might be suprised that in Uple or Variant we have
a (string*t) list instead of just a t list. 
That's because we associate a descriptive name to each type
appearing in a definition.

*)

type t= 
    Int 
   |String 
   |Uple of (string * t) list
   |List of string * t 
   |Array of string * t 
   |Record of (string * t) list
   |DisjunctionOfVariants of (string * t) list
   |Modularized of string * string ;;

