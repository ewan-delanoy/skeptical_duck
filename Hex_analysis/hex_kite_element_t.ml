(* 

#use"Hex_analysis/hex_kite_element_t.ml";;

*)

type t= 
   Active_cell of Hex_cell_t.t 
  |Bridge of Hex_cell_t.t * Hex_cell_t.t (* the pair is in Hex_cell.cmp order *)
  ;;

type increment_t =
    Eyed_claw of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t * Hex_cell_t.t
   |Noneyed_claw of Hex_double_hump_qualifier_t.t *  Hex_cardinal_direction_t.t * Hex_cell_t.t
   |Pyramid of  Hex_cardinal_direction_t.t * Hex_cell_t.t ;;

type new_whole_t =
    Old of t 
   |New of increment_t;;   