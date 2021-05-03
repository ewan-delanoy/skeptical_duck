(* 

#use"Ocaml_analysis/concrete_ocaml_type_t.ml";;

Polymorphic types not included at the moment.

*)

type t= 
    Int 
   |String 
   |Uple of t list
   |List of t 
   |Array of t 
   |Record of (string*t) list
   |Disjunction_of_variants of (string*(t list)) list 
   |Modular of string;;

