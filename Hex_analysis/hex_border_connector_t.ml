(* 

#use"Hex_analysis/hex_border_connector_t.ml";;

*)

type t= {
      comment : string ;
      entry_side : Hex_cardinal_direction_t.t ;
      connected_exit : Hex_cell_set_t.t ;
      junction : Hex_junction_t.t  ;
      other_active_cells : Hex_cell_set_t.t;
    } ;; 


