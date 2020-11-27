(* 

#use"Hex_analysis/hex_uc_report_t.ml";;

uc is for "unified connector"

*)

type t= {
      dimension : Hex_dimension_t.t;
         winner : Hex_player_t.t;
 ally_territory : Hex_cell_set_t.t;       
enemy_territory : Hex_cell_set_t.t;
free_territory  : Hex_cell_set_t.t;
         items  : Hex_ctct_report_item_t.t list;
         base   : Hex_base_of_connectors_t.t ;
     connectors : ((int * int) * Hex_unified_connector_t.t) list  
} ;;

