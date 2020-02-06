(* 

#use"Hex_analysis/hex_border_connector_name_t.ml";;

*)

type t= 
     Eyed_claw of     Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t 
    |Noneyed_claw of  Hex_double_hump_qualifier_t.t *  Hex_cardinal_direction_t.t 
    |Pyramid of       Hex_cardinal_direction_t.t
    |Small_pyramid of Hex_cardinal_direction_t.t  ;;