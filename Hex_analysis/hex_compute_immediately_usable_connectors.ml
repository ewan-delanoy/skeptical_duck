(* 

#use"Hex_analysis/hex_compute_immediately_usable_connectors.ml";;

*) 

module Private =struct 

let choose_unique_connector common connectors =
      if Hex_cell_set.length (common) >= 2 
      then let temp1 = Hex_cell_set.forget_order common in 
           let tempf = (fun k->List.nth temp1 (k-1)) in 
           Hex_generalized_connector_t.Bridge(tempf 1,tempf 2)   
      else Hex_generalized_connector_t.Named(List.hd connectors)     

end ;;   

let from_previous_items eob base ctct_report = 
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
       ((i,j),
        Private.choose_unique_connector common connectors)
   ) temp1 ;;


  