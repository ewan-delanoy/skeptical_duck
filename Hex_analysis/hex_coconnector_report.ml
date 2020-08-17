(* 

#use"Hex_analysis/hex_coconnector_report.ml";;

*) 


let draft_from_previous_items eob base report = 
   let formal_dim = eob.Hex_end_of_battle_t.dimension  
   and (Hex_ctct_report_t.R(l)) = report in 
   let indexed_l = Ennig.index_everything l in 
   let temp1 = Uple.list_of_pairs indexed_l in 
   Hex_coconnector_report_t.R(Image.image (
     fun ((i,item1),(j,item2))->
        let common = Hex_ctct_report_item.adjusted_common_neighbors formal_dim item1 item2 
        and connectors1 = Hex_base_of_connectors.select_coconnectors base item1 item2 in 
        let connectors = List.filter (
            fun nc->(Hex_named_connector.inner_sea nc)<> common 
        ) connectors1 in  
       ((i,j),
        (common,
         connectors))
   ) temp1);;
