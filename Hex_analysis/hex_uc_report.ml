(* 

#use"Hex_analysis/hex_uc_report.ml";;

*)

exception Lowercase_label_exn of int ;;

module Private = struct 

let one_step_constructor ctct_report l_base l_connectors = {
   Hex_uc_report_t. dimension = ctct_report.Hex_ctct_report_t.dimension ;
                       winner = ctct_report.Hex_ctct_report_t.winner ;
                       ally_territory = ctct_report.Hex_ctct_report_t.ally_territory;
                       enemy_territory = ctct_report.Hex_ctct_report_t.enemy_territory;
                       items  = ctct_report.Hex_ctct_report_t.items;
                       base   = l_base ;
                   connectors = l_connectors 
} ;;





let choose_unique_connector common connectors =
   if Hex_cell_set.length (common) >= 2 
   then let temp1 = Hex_cell_set.forget_order common in 
        let tempf = (fun k->List.nth temp1 (k-1)) in 
        Some(Hex_unified_connector_t.Bridge(tempf 1,tempf 2))   
   else match connectors with 
        []->None 
        |connector :: _ ->Some(Hex_unified_connector_t.Named(connector));;     

 

let compute_connectors eob base ctct_report = 
   let formal_dim = eob.Hex_end_of_battle_t.dimension  
   and l = ctct_report.Hex_ctct_report_t.items in 
   let indexed_l = Ennig.index_everything l in 
   let temp1 = Uple.list_of_pairs indexed_l in 
   Option.filter_and_unpack (
      fun ((i,item1),(j,item2))->
      let common = Hex_ctct_report_item.adjusted_common_neighbors formal_dim item1 item2 
      and connectors1 = Hex_base_of_connectors.select_coconnectors base item1 item2 in
      let connectors = List.filter (
         fun nc->(Hex_named_connector.inner_sea nc)<> common 
     ) connectors1 in  
     match choose_unique_connector common connectors with 
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



end ;;   

let from_previous_items eob =
    let ctct_report = Hex_ctct_report.about_end_of_battle eob in 
    let l_base = Hex_base_of_connectors.from_end_of_battle eob in 
    let l_connectors = Private.compute_connectors eob l_base ctct_report in 
    Private.constructor ctct_report l_base l_connectors ;;  

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
