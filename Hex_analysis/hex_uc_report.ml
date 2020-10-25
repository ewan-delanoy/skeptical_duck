(* 

#use"Hex_analysis/hex_uc_report.ml";;

*)

module Private = struct 

let constructor ctct_report l_connectors = {
   Hex_uc_report_t. dimension = ctct_report.Hex_ctct_report_t.dimension ;
                       winner = ctct_report.Hex_ctct_report_t.winner ;
              enemy_territory = ctct_report.Hex_ctct_report_t.enemy_territory;
                       items  = ctct_report.Hex_ctct_report_t.items;
                   connectors = l_connectors 
} ;;




let choose_unique_connector common connectors =
   if Hex_cell_set.length (common) >= 2 
   then let temp1 = Hex_cell_set.forget_order common in 
        let tempf = (fun k->List.nth temp1 (k-1)) in 
        Hex_unified_connector_t.Bridge(tempf 1,tempf 2)   
   else Hex_unified_connector_t.Named(List.hd connectors)     

 

let compute_connectors eob base ctct_report = 
   let formal_dim = eob.Hex_end_of_battle_t.dimension  
   and l = ctct_report.Hex_ctct_report_t.items in 
   let indexed_l = Ennig.index_everything l in 
   let temp1 = Uple.list_of_pairs indexed_l in 
   Image.image (
      fun ((i,item1),(j,item2))->
      let common = Hex_ctct_report_item.adjusted_common_neighbors formal_dim item1 item2 
      and connectors1 = Hex_base_of_connectors.select_coconnectors base item1 item2 in
      let connectors = List.filter (
         fun nc->(Hex_named_connector.inner_sea nc)<> common 
     ) connectors1 in  
    ((i,j),choose_unique_connector common connectors)
   ) temp1 ;;

end ;;   

let from_previous_items eob base =
    let ctct_report = Hex_ctct_report.about_end_of_battle eob in 
    let l_connectors = Private.compute_connectors eob base ctct_report in 
    Private.constructor ctct_report l_connectors ;;  

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
      ) uc_report.Hex_uc_report_t.enemy_territory in 
      let grid = {
         Hex_ascii_grid_t.beneficiary = uc_report.Hex_uc_report_t.winner ;
         dimension = uc_report.Hex_uc_report_t.dimension ;
         data = data1 @ data2;
      } in 
      print_string(Hex_visualize_grid.visualization grid);;
