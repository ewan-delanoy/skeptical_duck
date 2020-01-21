(* 

#use"Hex_analysis/hex_partial_kite_t.ml";;

The second argument is the set of cells involved in the kite.
The third argument says on which side of the board the kite started.

*)

type t= P of Hex_kite_element_t.t list * Hex_cell_set_t.t * Hex_cardinal_direction_t.t ;;