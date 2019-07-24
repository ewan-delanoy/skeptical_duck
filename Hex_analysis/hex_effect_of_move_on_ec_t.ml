(* 

#use"Hex_analysis/hex_effect_of_move_on_ec_t.ml";;

*)

type t= 
    Danger_disappeared
   |Critical_point_reached
   |Danger_increased of Hex_end_configuration_t.t;;