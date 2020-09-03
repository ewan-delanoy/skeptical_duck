(* 

#use"Hex_analysis/hex_polychrome.ml";;

*)


let initialize_with_ctct_report (Hex_ctct_report_t.R(l))=
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

let seek_mergeable_pair pochro =
   let temp1 = Uple.list_of_pairs pochro.Hex_polychrome_t.classes in 
   Option.find_and_stop (
      fun ((lbl1,(_,neighbors1)),(lbl2,(_,neighbors2))) -> 
         let common_neighbors = Hex_cell_set.intersect neighbors1 neighbors2 in 
         if Hex_cell_set.length(common_neighbors) <2 
         then None 
         else let l = Hex_cell_set.forget_order common_neighbors in 
              let gc = Hex_generalized_connector_t.Bridge(List.nth l 0,List.nth l 1) in 
              Some(lbl1,gc,lbl2) 
   ) temp1 ;;

let pusher walker =
  let (pochro,opt_action) = walker in 
  match opt_action with 
  None -> walker
  |Some(action) -> let new_pochro = add_new_mergeing pochro action in 
                   (new_pochro,seek_mergeable_pair new_pochro);;
   
let rec iterator walker =
    let (pochro,opt_action) = walker in   
    if opt_action = None 
    then pochro 
    else iterator(pusher walker) ;;  

let of_ctct_report ctct_report = 
   let pochro1 = initialize_with_ctct_report ctct_report in 
   let opener=(pochro1,seek_mergeable_pair pochro1) in 
   iterator opener;;

