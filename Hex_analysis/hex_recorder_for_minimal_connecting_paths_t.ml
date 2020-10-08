(* 

#use"Hex_analysis/hex_recorder_for_minimal_connecting_paths_t.ml";;

*)


type mapper_elt = (Hex_polychrome_label_t.t * Hex_polychrome_label_t.t) * (Hex_generalized_connector_t.t list)  ;;

type mapper = mapper_elt list ;;

type elt = (Hex_polychrome_label_t.t * Hex_polychrome_label_t.t) * 
      ((Hex_generalized_connector_t.t list) option) ;;

type t= { 
   paths : elt list ;
   connections : Hex_generalized_connector_t.t list;
};;