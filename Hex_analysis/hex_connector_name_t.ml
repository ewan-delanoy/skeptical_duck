(* 

#use"Hex_analysis/hex_connector_name_t.ml";;

*)

type t= 
      Inner of  Hex_inner_connector_name_t.t 
     |Border of Hex_border_connector_name_t.t ;;