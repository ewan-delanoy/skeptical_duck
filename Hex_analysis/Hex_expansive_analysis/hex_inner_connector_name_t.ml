(* 

#use"Hex_analysis/hex_inner_connector_name_t.ml";;

*)

type t= 
      Bridge of Hex_expsv_unit_side_t.t  
     |Broken_bridge of Hex_island_t.t * Hex_cell_t.t * Hex_cell_t.t * Hex_island_t.t
     |Typical of Hex_expsv_typical_inner_connector_name_t.t * Hex_cardinal_direction_t.t * bool;;

     