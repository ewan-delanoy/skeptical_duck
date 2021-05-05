(* 

#use"Ocaml_analysis/Standardized_concrete_ocaml_types/scct_record_or_variant_t.ml";;



*)

type t= 
  Record of (string*Scct_uple_level_t.t) list 
 |Variant of (string*Scct_uple_level_t.t) list ;;

