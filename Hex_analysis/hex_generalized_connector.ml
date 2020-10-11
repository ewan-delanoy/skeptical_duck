(* 

#use"Hex_analysis/hex_generalized_connector.ml";;

*)

let opt_constructor_in_half_checked_case (common,connectors) =
    if (Hex_cell_set.length common)>1
    then let l_common = Hex_cell_set.forget_order common in 
         let nth = (fun j->List.nth l_common (j-1)) in 
         (Some((nth 1,nth 2)),None)
    else      
    if   (connectors <> []) 
    then (None,Some(List.hd connectors))
    else (None,None);;

let support = function 
   Hex_generalized_connector_t.Bridge(cell1,cell2) -> Hex_cell_set.safe_set [cell1;cell2]
  |Named(nc) -> Hex_named_connector.inner_sea nc;; 