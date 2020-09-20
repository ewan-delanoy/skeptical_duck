(* 

#use"Hex_analysis/hex_recorder_for_minimal_connecting_paths_t.ml";;

*)

type t= R of 
   ( (Hex_polychrome_label_t.t * Hex_polychrome_label_t.t) * 
      ((Hex_generalized_connector_t.t list) option) list)
 ;;