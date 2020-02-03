(* 

#use"Hex_analysis/hex_connector_name_t.ml";;

*)

type t= 
     Bridge of Hex_unit_side_t.t 
    |Eyed_claw of Hex_borderwise_t.t * Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t 
    |Noneyed_claw of Hex_borderwise_t.t * Hex_double_hump_qualifier_t.t *  Hex_cardinal_direction_t.t 
    |Pyramid of  Hex_borderwise_t.t * Hex_cardinal_direction_t.t
    |Small_pyramid of Hex_borderwise_t.t * Hex_cardinal_direction_t.t  ;;