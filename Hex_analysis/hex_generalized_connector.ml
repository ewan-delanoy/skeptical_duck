(* 

#use"Hex_analysis/hex_generalized_connector.ml";;

*)

let constructor common connectors = 
   Hex_generalized_connector_t.G(common,connectors);;

let is_not_empty (Hex_generalized_connector_t.G(common,connectors)) =
      ((common,connectors) <> (Hex_cell_set.empty_set,[])) ;;

let is_strong (Hex_generalized_connector_t.G(common,connectors)) =
      ((Hex_cell_set.length common)>1) || (connectors <> []) ;;

let verify (Hex_generalized_connector_t.G(neighbors,connectors)) item= 
    let n1 = Hex_cell_set.length neighbors 
    and n2 = List.length connectors in 
    if (n1 > 0) && (n2 > 0)
              then Some("Both neighbors and connectors",item) else
    if n1 > 2 then Some("More than two neighbors",item) else 
    None ;;      