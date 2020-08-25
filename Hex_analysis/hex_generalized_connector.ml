(* 

#use"Hex_analysis/hex_generalized_connector.ml";;

*)

(*
let constructor common connectors = 
   Hex_generalized_connector_t.G(common,connectors);;
*)

let opt_constructor_in_half_checked_case (common,connectors) =
    if (Hex_cell_set.length common)>1
    then let l_common = Hex_cell_set.forget_order common in 
         let nth = (fun j->List.nth l_common (j-1)) in 
         Some(Hex_generalized_connector_t.Bridge(nth 1,nth 2))
    else      
    if   (connectors <> []) 
    then Some(Hex_generalized_connector_t.Named(List.hd connectors))
    else None;;
