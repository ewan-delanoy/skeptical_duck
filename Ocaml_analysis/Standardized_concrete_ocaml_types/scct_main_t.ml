(* 

#use"Ocaml_analysis/Standardized_concrete_ocaml_types/scct_main_t.ml";;



*)

type t= {
   modulename : string ;
   shortname : string ;
   kind : Scct_either_variant_or_record_t.t ;
   data : Scct_possibly_listy_uple_t.t list ;
} ;;

