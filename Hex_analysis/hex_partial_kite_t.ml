(* 

#use"Hex_analysis/hex_partial_kite_t.ml";;

The second argument is the set of cells involved in the kite 

*)

type t= P of Hex_kite_element_t.t list * Hex_cell_set_t.t ;;