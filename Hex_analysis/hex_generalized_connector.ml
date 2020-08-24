(* 

#use"Hex_analysis/hex_generalized_connector.ml";;

*)

let constructor common connectors = 
   Hex_generalized_connector_t.G(common,connectors);;


let opt_constructor_in_half_checked_case (common,connectors) =
    if   ((Hex_cell_set.length common)>1) || (connectors <> []) 
    then Some(Hex_generalized_connector_t.G(common,connectors))
    else None;;
