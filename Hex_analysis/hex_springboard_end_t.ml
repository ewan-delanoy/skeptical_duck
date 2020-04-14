(* 

#use"Hex_analysis/hex_springboard_end_t.ml";;

*)

type t = {
   alternative_move : Hex_cell_t.t ;
   is_final : bool ;
   extra_content : Hex_named_connector_t.t ;
} ;;
