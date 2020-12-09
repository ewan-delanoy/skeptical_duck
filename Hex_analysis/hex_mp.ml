(* 

#use"Hex_analysis/hex_mp.ml";;

mp is for "maximal paths"

*)

let cells_from_components ctct_items (Hex_mp_t.MP l) =
    let unordered_indices = Image.image (fun (_,Hex_ctct_index_t.I k)->k) (List.flatten l) in 
    let indices = Ordered.sort Total_ordering.for_integers unordered_indices in 
    Hex_cell_set.fold_merge (Image.image (fun 
       k -> let item = List.nth ctct_items (k-1) in 
       item.Hex_ctct_report_item_t.active_dwellers
    ) indices);;


let cells_from_junctions (Hex_mp_t.MP l) = 
Hex_cell_set.fold_merge (Image.image 
      ( fun (uc,_)->Hex_unified_connector.support uc )
   (List.flatten l));;

