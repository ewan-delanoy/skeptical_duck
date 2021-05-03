(* 

#use"Ocaml_analysis/Standardized_concrete_ocaml_types/scct_atomic_type_t.ml";;

"Short", simple types for which converters to and from concrete objects 
have already been constructed.

*)

type t= 
    Bool
   |Int 
   |Int_List
   |Int_Pair
   |Int_Triple 
   |String 
   |String_List
   |String_Pair
   |String_Pair_List
   |String_Triple 
   |String_List_List
   |Modular of string;;

