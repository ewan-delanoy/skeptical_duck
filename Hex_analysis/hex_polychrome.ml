(* 

#use"Hex_analysis/hex_polychrome.ml";;

*)


let initialize_with_previous_data eob ctct_report=
    let l_report = ctct_report.Hex_ctct_report_t.items in  
    let all_free_cells = Hex_end_of_battle.remaining_free_cells eob in 
    let temp1 = Ennig.index_everything l_report in 
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
      Hex_polychrome_t.dimension = eob.Hex_end_of_battle_t.dimension ;
      winner     = eob.Hex_end_of_battle_t.winner;
      enemy_territory = ctct_report.Hex_ctct_report_t.enemy_territory ;
      classes    = the_classes ;
      labels     = the_labels ;
      free_cells = all_free_cells ;
      recorder    = (Hex_recorder_for_minimal_connecting_paths.empty_one (List.length l_report));
    };;


let add_new_mergeing pochro unlabeled_action =
    let (husband,gc,wife) = unlabeled_action in 
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
      pochro with 
      Hex_polychrome_t.classes    = new_classes ;
      labels     = new_labels ;
      free_cells = Hex_cell_set.setminus (pochro.Hex_polychrome_t.free_cells) absorbed_ones ;
      recorder    =  Hex_recorder_for_minimal_connecting_paths.add_merger 
                  (pochro.Hex_polychrome_t.recorder) unlabeled_action;
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

let pusher_for_pair_mergeings walker =
  let (pochro,opt_action) = walker in 
  match opt_action with 
  None -> walker
  |Some(action) -> let new_pochro = add_new_mergeing pochro action in 
                   (new_pochro,seek_mergeable_pair new_pochro);;
   
let rec iterator_for_pair_mergeings walker =
    let (pochro,opt_action) = walker in   
    if opt_action = None 
    then pochro 
    else iterator_for_pair_mergeings(pusher_for_pair_mergeings walker) ;;  

let add_all_possible_pair_mergeings eob ctct_report = 
   let pochro1 = initialize_with_previous_data eob ctct_report in 
   let opener=(pochro1,seek_mergeable_pair pochro1) in 
   iterator_for_pair_mergeings opener;;


let seek_usual_connector_for_individual_pair player base pochro (i,j)=
   let the_classes = pochro.Hex_polychrome_t.classes in 
   let (component,_) = List.assoc (Hex_polychrome_label_t.L(j)) the_classes in 
   let side1 = Hex_order_for_player_sides.select player i in 
   let island1 = Hex_island.bare_side side1 
   and island2 = Hex_island_t.I(Hex_anchor_t.No_anchor,
   Set_of_poly_pairs.safe_set(Hex_cell_set.image Hex_cell.to_int_pair component )) 
   and ether = pochro.Hex_polychrome_t.free_cells in 
   let li = Hex_polychrome_label_t.L(i)
   and lj = Hex_polychrome_label_t.L(j) in 
   match Hex_base_of_connectors.select_usual_connectors base (island1,ether,island2) with 
   [] -> (
           match Hex_base_of_connectors.select_usual_connectors base (island2,ether,island1) with 
           [] -> None 
           |sol::_ -> Some (li,Hex_generalized_connector_t.Named sol,lj)   
         )
   |sol2::_ -> Some (lj,Hex_generalized_connector_t.Named sol2,li) ;;      

let all_usual_connectors player base pochro =
   let the_classes = pochro.Hex_polychrome_t.classes in 
   let class_indices = Image.image (fun (Hex_polychrome_label_t.L(k),_)->k) the_classes in 
   let ipairs = Uple.list_of_pairs class_indices in 
   Option.filter_and_unpack (seek_usual_connector_for_individual_pair player base pochro) ipairs;;
  

let pusher_for_usual_mergeings walker =
  let (player,base,pochro,remaining_untreated_actions) = walker in 
  match remaining_untreated_actions with 
   [] -> walker
  |action::other_actions -> let new_pochro = add_new_mergeing pochro action in 
                   (player,base,new_pochro,other_actions);;
   
let rec iterator_for_usual_mergeings walker =
    let (player,base,pochro,remaining_untreated_actions) = walker in   
    if remaining_untreated_actions = [] 
    then pochro 
    else iterator_for_usual_mergeings(pusher_for_usual_mergeings walker) ;;  

let add_all_possible_usual_mergeings player base pochro = 
   let opener=(player,base,pochro,all_usual_connectors player base pochro) in 
   iterator_for_usual_mergeings opener;;

let from_previous_data eob base ctct_report=
   let pochro1 = add_all_possible_pair_mergeings eob ctct_report 
   and player = eob.Hex_end_of_battle_t.winner in 
   add_all_possible_usual_mergeings player base pochro1;;    


let connections pochro = 
  let defs =(pochro.Hex_polychrome_t.recorder).Hex_recorder_for_minimal_connecting_paths_t.definitions_for_new_labels in 
  Image.image (fun (_,(_,gc,_))->gc) defs ;;
  

let visualization pochro =
   let cti = Hex_cell.to_int_pair in 
   let data1 = Image.image (
      fun (cell,Hex_polychrome_label_t.L(i)) ->
        (cti cell,Hex_visualize_grid.int_in_cell i)
   ) pochro.Hex_polychrome_t.labels in
   let data2 = Hex_cell_set.image (
       fun cell ->(cti cell,"EEE") 
   ) pochro.Hex_polychrome_t.enemy_territory in 
   let temp1 =Ennig.index_everything(List.rev(connections pochro)) in 
   let data3 = List.flatten(Image.image (
      fun (idx,gc)->
        let label = Hex_visualize_grid.label_in_cell idx in 
        Hex_cell_set.image (fun cell->(cti cell,label))
        (Hex_generalized_connector.support gc)
   ) temp1) in 
   let grid = {
      Hex_ascii_grid_t.beneficiary = pochro.Hex_polychrome_t.winner ;
      dimension = pochro.Hex_polychrome_t.dimension ;
      data = data1 @ data2 @ data3 ;
   } in 
   Hex_visualize_grid.visualization grid ;;

let  visualize pochro =  
   print_string(visualization pochro);; 

let print_out (fmt:Format.formatter) pochro=
   Format.fprintf fmt "@[%s@]" (visualization pochro);;     






