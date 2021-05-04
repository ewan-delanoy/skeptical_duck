(* 

#use"Ocaml_analysis/Standardized_concrete_ocaml_types/scct_uple_level_t.ml";;

An element in the uple is represented by a string * bool * Scct_atomic_type_t.t object.

The string is used for variable naming, the bool indicates whether we have a list or not.

Similary, the leftmost bool in the definition indicates wether we have 
an uple or a list of uples.

*)

type t= U of bool * ((string * bool * Scct_atomic_type_t.t ) list) ;;

