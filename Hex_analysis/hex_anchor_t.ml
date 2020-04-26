(* 

#use"Hex_analysis/hex_anchor_t.ml";;

Data describing the border component of a Hex_island_t.t object

*)

type t= 
     No_anchor 
    |Single_anchor of Hex_cardinal_direction_t.t
    |Double_anchor of Hex_cardinal_direction_t.t * Hex_cardinal_direction_t.t (* occurs rarely *) ;;