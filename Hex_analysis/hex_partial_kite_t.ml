(* 

#use"Hex_analysis/hex_partial_kite_t.ml";;

The second argument is the set of cells involved in the kite.
The third argument says on which side of the board the kite started.


*)

type old_t= P of Hex_kite_element_t.old_t list * Hex_cell_set_t.t * Hex_cardinal_direction_t.t ;;


type t={
   stops_so_far : Hex_kite_element_t.t list;
   original_side : Hex_cardinal_direction_t.t;
   unvisited_islands : Hex_island_t.t list;
   unvisited_seas : Hex_named_connector_t.t list;
};;