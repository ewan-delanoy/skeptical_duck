(* 

#use"Ocaml_analysis/Standardized_concrete_ocaml_types/scct_either_from_or_to_crobj.ml";;


*)

let argument_name = function 
     Scct_either_from_or_to_crobj_t.From_crobj -> "crobj"
    |To_crobj -> "x";;

let to_string = function 
     Scct_either_from_or_to_crobj_t.From_crobj -> "from"
    |To_crobj -> "to";;

