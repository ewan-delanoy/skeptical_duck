(* 

#use"Hex_analysis/hex_border_connector_name_t.ml";;

*)

type t= 
     Eyed_claw of  Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t 
    |Typical of    Hex_typical_border_connector_name_t.t * Hex_cardinal_direction_t.t ;;