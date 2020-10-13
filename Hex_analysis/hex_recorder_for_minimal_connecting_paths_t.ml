(* 

#use"Hex_analysis/hex_recorder_for_minimal_connecting_paths_t.ml";;

*)


type mapper_elt = (Hex_polychrome_label_t.t * Hex_polychrome_label_t.t) * (Hex_generalized_connector_t.t list)  ;;

type mapper = mapper_elt list ;;

type t= { 
   number_of_old_labels : int; 
   mapper : mapper ;
   bridges_for_new_labels : (Hex_polychrome_label_t.t * (Hex_generalized_connector_t.t list)) list;
   contents_for_new_labels : (Hex_polychrome_label_t.t * (Hex_polychrome_label_t.t list)) list;
   definitions_for_new_labels : (Hex_polychrome_label_t.t * (Hex_polychrome_label_t.t * Hex_generalized_connector_t.t *
      Hex_polychrome_label_t.t)) list;
};;