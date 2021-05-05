(* 

#use"Ocaml_analysis/Standardized_concrete_ocaml_types/scct_record_or_variant_t.ml";;



*)

type t= {
   modulename : string ;
   is_variant : bool ;
   data : (string*Scct_uple_level_t.t) list ;
} ;;

