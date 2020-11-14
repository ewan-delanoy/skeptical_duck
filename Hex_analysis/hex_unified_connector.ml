(* 

#use"Hex_analysis/hex_unified_connector.ml";;

*)

let bounds_for_authorized_translations dim uc = match uc with 
  (Hex_unified_connector_t.Bridge(plyr,(cell1,cell2)))-> 
     Hex_ipair.bounds_for_authorized_translations dim 
        (Image.image Hex_cell.to_int_pair [cell1;cell2])   
 |Named(nc) ->Hex_named_connector.bounds_for_authorized_translations dim nc ;;
 

let oppflect dim uc = match uc with 
  (Hex_unified_connector_t.Bridge(plyr,(cell1,cell2)))-> 
     Hex_unified_connector_t.Bridge(Hex_player.other_player plyr,(Hex_cell_isometry.oppflect dim cell1,Hex_cell_isometry.oppflect dim cell2))    
 |Named(nc) ->Named (Hex_named_connector.oppose dim (Hex_named_connector.reflect nc));;   


let oppose dim uc = match uc with 
  (Hex_unified_connector_t.Bridge(plyr,(cell1,cell2)))-> 
      Hex_unified_connector_t.Bridge(plyr,(Hex_cell_isometry.oppose dim cell1,Hex_cell_isometry.oppose dim cell2))    
 |Named(nc) ->Named (Hex_named_connector.oppose dim nc);;  

 let opt_constructor_in_half_checked_case plyr (common,connectors) =
    if (Hex_cell_set.length common)>1
    then let l_common = Hex_cell_set.forget_order common in 
         let nth = (fun j->List.nth l_common (j-1)) in 
         Some(Hex_unified_connector_t.Bridge(plyr,(nth 1,nth 2)))
    else      
    if   (connectors <> []) 
    then Some(Hex_unified_connector_t.Named(List.hd connectors))
    else None;;

let reflect uc = match uc with 
 (Hex_unified_connector_t.Bridge(plyr,(cell1,cell2)))-> 
    Hex_unified_connector_t.Bridge(Hex_player.other_player plyr,(Hex_cell_isometry.reflect cell1,Hex_cell_isometry.reflect cell2))    
|Named(nc) ->Named (Hex_named_connector.reflect nc);;

let support = function 
   Hex_unified_connector_t.Bridge(_,(cell1,cell2)) -> Hex_cell_set.safe_set [cell1;cell2]
  |Named(nc) -> Hex_named_connector.inner_sea nc;; 

let translate dv = function 
 (Hex_unified_connector_t.Bridge(plyr,(cell1,cell2)))-> 
   Hex_unified_connector_t.Bridge(plyr,(Hex_cell_isometry.translate dv cell1,Hex_cell_isometry.translate dv cell2))    
 |Named(nc) ->Named (Hex_named_connector.translate dv nc);;
 

