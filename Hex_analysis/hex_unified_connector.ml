(* 

#use"Hex_analysis/hex_unified_connector.ml";;

*)

let opt_constructor_in_half_checked_case plyr (common,connectors) =
    if (Hex_cell_set.length common)>1
    then let l_common = Hex_cell_set.forget_order common in 
         let nth = (fun j->List.nth l_common (j-1)) in 
         Some(Hex_unified_connector_t.Bridge(plyr,(nth 1,nth 2)))
    else      
    if   (connectors <> []) 
    then Some(Hex_unified_connector_t.Named(List.hd connectors))
    else None;;

let support = function 
   Hex_unified_connector_t.Bridge(_,(cell1,cell2)) -> Hex_cell_set.safe_set [cell1;cell2]
  |Named(nc) -> Hex_named_connector.inner_sea nc;; 