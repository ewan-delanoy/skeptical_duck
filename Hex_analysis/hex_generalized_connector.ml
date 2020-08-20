(* 

#use"Hex_analysis/hex_generalized_connector.ml";;

*)

let constructor common connectors = 
   Hex_generalized_connector_t.G(common,connectors);;

let is_not_empty (Hex_generalized_connector_t.G(common,connectors)) =
      ((common,connectors) <> (Hex_cell_set.empty_set,[])) ;;