(* 

#use"Hex_analysis/hex_uc_report.ml";;

*)

exception Lowercase_label_exn of int ;;

module Private = struct 


let is_a_corner_cell formal_dim cell =
     let (Hex_dimension_t.D dim) = formal_dim 
     and (i,j) = Hex_cell.to_int_pair cell in 
     (List.mem i [1;dim]) &&  (List.mem j [1;dim]) ;;

let has_a_corner_cell formal_dim nc =
    let (Hex_cell_set_t.S l) = Hex_named_connector.inner_sea nc in 
     List.exists (is_a_corner_cell formal_dim) l;;

let prefer_corner_cells formal_dim l_uc =
     let l2_uc = List.filter (has_a_corner_cell formal_dim) l_uc in 
     if l2_uc <> [] then l2_uc else l_uc ;;

let minimize_wrt_inner_area l =
    match l with
    [] -> []
    | _ -> snd(Min.minimize_it_with_care Hex_named_connector.inner_area l);;

   

let choose_unique_connector plyr common connectors =
   if Hex_cell_set.length (common) >= 2 
   then let temp1 = Hex_cell_set.forget_order common in 
        let tempf = (fun k->List.nth temp1 (k-1)) in 
        Some(Hex_unified_connector_t.Bridge(plyr,(tempf 1,tempf 2)))   
   else match connectors with 
        []->None 
        |connector :: _ ->Some(Hex_unified_connector_t.Named(connector));;     

 

let compute_connectors eob base ctct_report = 
   let formal_dim = eob.Hex_end_of_battle_t.dimension 
   and plyr = eob.Hex_end_of_battle_t.winner 
   and l = ctct_report.Hex_ctct_report_t.items in 
   let indexed_l = Ennig.index_everything l in 
   let temp1 = Uple.list_of_pairs indexed_l in 
   Option.filter_and_unpack (
      fun ((i,item1),(j,item2))->
      let common = Hex_ctct_report_item.adjusted_common_neighbors formal_dim item1 item2 
      and connectors1 = Hex_base_of_connectors.select_coconnectors base item1 item2 in
      let connectors2 = List.filter (
         fun nc->(Hex_named_connector.inner_sea nc)<> common 
     ) connectors1 in  
      let connectors3 = prefer_corner_cells formal_dim connectors2 in 
      let connectors4 = minimize_wrt_inner_area connectors3 in 
     match choose_unique_connector (formal_dim,plyr) common connectors4 with 
     None -> None 
     | Some(nc)->Some((i,j),nc)
   ) temp1 ;;

let lowercase_label k = 
   if (k<1)||(k>26) 
   then raise(Lowercase_label_exn(k)) 
   else " "^(String.make 1 (char_of_int(k+96)))^" ";;

let colouring_for_connectors ucr = 
    let temp1 = Image.image (fun (_,uc)->Hex_unified_connector.support uc) ucr.Hex_uc_report_t.connectors in 
    let temp2 = Ennig.index_everything temp1 in 
    let temp3 = Image.image (fun (k,domain)->(lowercase_label k,domain) ) temp2 in 
    let whole = Hex_cell_set.fold_merge temp1 in 
    let temp4 = Hex_cell_set.image (
      fun cell ->
        let ttemp5 = List.filter (fun (k,domain)->Hex_cell_set.mem cell domain) temp3 in
        let label =(
            if List.length(ttemp5)=1
            then fst(List.hd ttemp5)
            else "***") in 
        (Hex_cell.to_int_pair cell,label)         
    ) whole in 
    temp4 ;;

let one_step_constructor ctct_report =
      let eob = Hex_ctct_report.to_end_of_battle ctct_report in 
      let l_base = Hex_base_of_connectors.from_end_of_battle eob in 
     let l_connectors = compute_connectors eob l_base ctct_report in 
     {
    Hex_uc_report_t. dimension = ctct_report.Hex_ctct_report_t.dimension ;
                        winner = ctct_report.Hex_ctct_report_t.winner ;
                ally_territory = ctct_report.Hex_ctct_report_t.ally_territory;
               enemy_territory = ctct_report.Hex_ctct_report_t.enemy_territory;
                        items  = ctct_report.Hex_ctct_report_t.items;
                        base   = l_base ;
                    connectors = l_connectors 
    } ;;

let one_step_forgetter uc_report =
     {
       Hex_ctct_report_t. dimension = uc_report.Hex_uc_report_t.dimension ;
                             winner = uc_report.Hex_uc_report_t.winner ;
                     ally_territory = uc_report.Hex_uc_report_t.ally_territory;
                    enemy_territory = uc_report.Hex_uc_report_t.enemy_territory;
                             items  = uc_report.Hex_uc_report_t.items;                 
    } ;;


end ;;   


let cumulative_constructor fg =
    let ctct_report = Hex_ctct_report.constructor fg in 
    Private.one_step_constructor ctct_report  ;;  

let forbidden_composite_patterns = [
   Hex_composite_pattern_t.C (Hex_pattern_t.Pat [((5, 4), true); ((6, 2), true)],
   Hex_unified_connector_t.Named
    {Hex_named_connector_t.name =
      Hex_connector_name_t.Border
       (Hex_border_connector_name_t.Eyed_claw (Hex_cardinal_direction_t.Up,
         Hex_cardinal_direction_t.Right));
     entry = (Hex_island_t.I(Hex_anchor_t.No_anchor,Set_of_poly_pairs_t.S [(4, 3)]));
     junction =
      [(1, 1); (1, 2); (1, 3); (1, 4); (1, 5); (1, 6); (1, 7); (2, 1); 
       (2, 2); (2, 3); (2, 4); (2, 5); (2, 6); (3, 1); (3, 2); (3, 3); 
       (3, 4); (3, 5); (4, 2)];
     exit = (Hex_island_t.I(Hex_anchor_t.Single_anchor Hex_cardinal_direction_t.Up,
     Set_of_poly_pairs_t.S [])); apex = Some (4, 3)})
] ;;

let one_step_constructor = Private.one_step_constructor ;;

let one_step_forgetter = Private.one_step_forgetter ;;

let visualize uc_report =
      let cti = Hex_cell.to_int_pair in 
      let indexed_items= Ennig.index_everything uc_report.Hex_uc_report_t.items in 
      let data1 = List.flatten (Image.image (
         fun (j,item) ->
            Hex_cell_set.image (
               fun cell->(cti cell,Hex_visualize_grid.int_in_cell j)
            ) item.Hex_ctct_report_item_t.active_dwellers
           
      ) indexed_items) in 
      let data2 = Hex_cell_set.image (
          fun cell ->(cti cell,"EEE") 
      ) uc_report.Hex_uc_report_t.enemy_territory 
      and data3 = Private.colouring_for_connectors uc_report in 
      let grid = {
         Hex_ascii_grid_t.beneficiary = uc_report.Hex_uc_report_t.winner ;
         dimension = uc_report.Hex_uc_report_t.dimension ;
         data = data1 @ data2 @ data3;
      } in 
      print_string(Hex_visualize_grid.visualization grid);;
