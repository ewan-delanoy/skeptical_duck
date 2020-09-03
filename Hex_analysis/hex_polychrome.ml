(* 

#use"Hex_analysis/hex_polychrome.ml";;

*)

module Private = struct 

let add_new_mergeing pochro new_action =
    let (husband,gc,wife) = new_action in 
    let absorbed_ones = Hex_generalized_connector.support gc in 
    let new_index = (Max.list(Image.image (fun (_,Hex_polychrome_label_t.L(i))->i) 
        pochro.Hex_polychrome_t.labels))+1 in 
    let new_lbl = Hex_polychrome_label_t.L new_index in     
    let new_labels = Option.filter_and_unpack (
       fun (cell,lbl) -> 
         if Hex_cell_set.mem cell absorbed_ones 
         then None
         else 
         if List.mem lbl [husband;wife]
         then Some(cell,new_lbl)
         else Some(cell,lbl)
    ) pochro.Hex_polychrome_t.labels in 
    let old_classes = pochro.Hex_polychrome_t.classes in 
    let (husband_site,husband_neighbors) = List.assoc husband old_classes 
    and (wife_site,wife_neighbors) = List.assoc wife old_classes in
    let merged_site = Hex_cell_set.fold_merge [husband_site ; absorbed_ones ; wife_site] in
    let merged_neighbors = Hex_cell_set.setminus 
       (Hex_cell_set.merge husband_neighbors wife_neighbors) absorbed_ones in 
    let new_classes = (List.filter (fun (lbl,_)->
         not(List.mem lbl [husband;wife])
    ) old_classes) @ [new_lbl,(merged_site,merged_neighbors)] in 
     {
      Hex_polychrome_t.classes    = new_classes ;
      labels     = new_labels ;
      free_cells = Hex_cell_set.setminus (pochro.Hex_polychrome_t.free_cells) absorbed_ones ;
      history    = new_action :: (pochro.Hex_polychrome_t.history);
    };;
 

let of_ctct_report (Hex_ctct_report_t.R(l))=
    let all_free_cells = Hex_cell_set.fold_merge 
      (Image.image (fun item->item.Hex_ctct_report_item_t.passive_neighbors) l) 
     in 
    let temp1 = Ennig.index_everything l in 
    let the_classes =   Image.image (
       fun (idx,item) ->
         (Hex_polychrome_label_t.L(idx),
          (item.Hex_ctct_report_item_t.active_dwellers,item.Hex_ctct_report_item_t.passive_neighbors))
    ) temp1 
    and temp2 = List.flatten(Image.image (
       fun (idx,item) ->
         Hex_cell_set.image (fun cell->(cell,Hex_polychrome_label_t.L(idx))) 
          item.Hex_ctct_report_item_t.active_dwellers
    ) temp1) in 
    let all_active_cells = Hex_cell_set.safe_set (Image.image fst temp2) in 
    let the_labels = Hex_cell_set.image (
      fun cell ->  (cell,List.assoc cell temp2)
    ) all_active_cells in 
    {
      Hex_polychrome_t.classes    = the_classes ;
      labels     = the_labels ;
      free_cells = all_free_cells ;
      history    = [];
    };;

end ;; 

(*

type t= {
    classes    : (Hex_polychrome_label_t.t * Hex_cell_set_t.t) list ;
    labels     : (Hex_cell_t.t * Hex_polychrome_label_t.t) list ;
    free_cells : Hex_cell_set_t.t ;
    history    : (Hex_polychrome_label_t.t * Hex_generalized_connector_t.t * Hex_polychrome_label_t.t) list;
};;

*)