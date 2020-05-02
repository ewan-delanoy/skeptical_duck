(* 

#use"Hex_analysis/hex_partial_kite_t.ml";;

The second argument is the set of cells involved in the kite.
The third argument says on which side of the board the kite started.


*)


type t={
   place_of_birth : Hex_island_t.t ;
   steps_so_far :  Hex_kite_element_t.t list ;
   unvisited_islands : Hex_island_t.t list;
   unvisited_seas : (( Hex_cell_set_t.t * Hex_named_connector_t.t) list) ;
   added_by_casing : Hex_cell_set_t.t ;
   remaining_free_cells : Hex_cell_set_t.t ;
};;