(* 
#use"Hex_analysis/hex_generalized_connector_t.ml";;
*)



type t= 
   Bridge of Hex_simple_bridge_t.t
  |Named of  Hex_named_connector_t.t ;;

