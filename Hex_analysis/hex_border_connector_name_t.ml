(* 

#use"Hex_analysis/hex_border_connector_name_t.ml";;

*)

type t= 
     Eyed_claw of     Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t 
    |Bs_D
    |Bs_L
    |Bs_R
    |Bs_U
    |Sb_D
    |Sb_L
    |Sb_R
    |Sb_U
    |Pyramid of       Hex_cardinal_direction_t.t
    |Small_pyramid of Hex_cardinal_direction_t.t  
    |Border_bridge of Hex_cardinal_direction_t.t
    |Walleye1 of Hex_cardinal_direction_t.t;;