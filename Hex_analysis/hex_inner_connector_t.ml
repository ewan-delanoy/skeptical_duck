(* 

#use"Hex_analysis/hex_inner_connector_t.ml";;

*)

type t= {
      comment : string ;
      connected_entry : Hex_cell_set_t.t ;
      connected_exit : Hex_cell_set_t.t ;
      junction : (Hex_cell_set_t.t * Hex_cell_set_t.t) list ;
      other_active_cells : Hex_cell_set_t.t;
    } ;; 


