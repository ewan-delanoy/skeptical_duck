(* 

#use"Hex_analysis/hex_ctct_report_t.ml";;

*)

type t= {
      dimension : Hex_dimension_t.t;
         winner : Hex_player_t.t;
 ally_territory : Hex_cell_set_t.t;     
enemy_territory : Hex_cell_set_t.t;
         items  : Hex_ctct_report_item_t.t list;
} ;;
