(* 

#use"Hex_analysis/hex_simple_bridge_t.ml";;

*)

type half_t = {
    start_side : Hex_cell_t.t ;
    bridger :  Hex_cell_t.t ;
    end_side : Hex_cell_t.t 
};;

type t = {
    left_half : half_t ;
    right_half : half_t ;
};;

