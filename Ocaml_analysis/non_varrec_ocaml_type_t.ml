(* 

#use"Ocaml_analysis/non_varrec_ocaml_type_t.ml";;

A varrec type is a disjunction of VARiants type or a RECord.
Here we also exclude uples because in our standardized forms for
ADT's, uples do not appear alone but only inside variants.

*)

type t= 
    Int 
   |String 
   |List of t 
   |Array of t 
   |Modular of string;;

