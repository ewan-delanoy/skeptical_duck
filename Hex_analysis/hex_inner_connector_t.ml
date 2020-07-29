(* 

#use"Hex_analysis/hex_inner_connector_t.ml";;

*)

type t= {
      comment : string ;
      connected_entry : Hex_cell_set_t.t ;
      connected_exit : Hex_cell_set_t.t ;
      junction : Hex_junction_t.t ;
      other_active_cells : Hex_cell_set_t.t;
    } ;; 


