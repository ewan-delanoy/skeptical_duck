(* 

#use"Hex_analysis/hex_kite_element_t.ml";;

*)

type t= 
   Active_cell of Hex_cell_t.t 
  |Bridge of Hex_cell_t.t * Hex_cell_t.t (* the pair is in Hex_cell.cmp order *)
  ;;