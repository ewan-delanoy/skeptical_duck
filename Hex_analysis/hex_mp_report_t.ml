(* 

#use"Hex_analysis/hex_mp_report_t.ml";;

mp is for "maximal paths"

*)

type t= {
      dimension   : Hex_dimension_t.t;
         winner   : Hex_player_t.t;
   ally_territory : Hex_cell_set_t.t;       
enemy_territory   : Hex_cell_set_t.t;
  free_territory  : Hex_cell_set_t.t;
         items    : Hex_ctct_report_item_t.t list;
         base     : Hex_base_of_connectors_t.t ;
       connectors : ((Hex_ctct_index_t.t * Hex_ctct_index_t.t) * Hex_unified_connector_t.t) list ; 
     paths_from_1 : (int * (Hex_unified_connector_t.t * int) list) list ;
     paths_from_2 : (int * (Hex_unified_connector_t.t * int) list) list ;  
} ;;
