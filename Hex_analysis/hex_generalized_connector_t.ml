(* 
#use"Hex_analysis/hex_generalized_connector_t.ml";;
*)

type half_bridge_t = {
    start_side : Hex_cell_t.t ;
    bridger :  Hex_cell_t.t ;
    end_side : Hex_cell_t.t 
};;

type bridge_t = {
    left_half : half_bridge_t ;
    right_half : half_bridge_t ;
};;

type t= 
   Bridge of bridge_t
  |Named of  Hex_named_connector_t.t ;;

  