(* 

#use"Hex_analysis/hex_kite_element_t.ml";;

*)

type t =
    Earth of Hex_island_t.t 
   |Sea   of Hex_expsv_named_connector_t.t 
   |Springboard of Hex_expsv_springboard_t.t ;; 