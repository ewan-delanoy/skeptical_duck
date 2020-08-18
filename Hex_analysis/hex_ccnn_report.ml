(* 

#use"Hex_analysis/hex_coconnector_report.ml";;

*) 


module Private = struct 

let first_draft_from_previous_items eob base ctct_report = 
   let formal_dim = eob.Hex_end_of_battle_t.dimension  
   and (Hex_ctct_report_t.R(l)) = ctct_report in 
   let indexed_l = Ennig.index_everything l in 
   let temp1 = Uple.list_of_pairs indexed_l in 
   Hex_ccnn_report_t.R(Image.image (
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

let deduce_removabilities_from_pattern
   (Hex_ctct_report_t.R(l_report)) 
    (Hex_ccnn_report_t.R draft) (Hex_pattern_t.Pat l_pat) = 
   let points = Option.filter_and_unpack (
     fun (p,is_active) -> 
      if is_active 
      then Some(Hex_cell.of_int_pair p)
       else None 
   ) l_pat in 
   let possibly_removable_point = List.nth points 2 in 
   let (pr_idx,_) = Listennou.force_find (fun (idx,item) ->
     Hex_cell_set.mem possibly_removable_point (item.Hex_ctct_report_item_t.active_dwellers)
   ) (Ennig.index_everything l_report) in  
   let relevant_coconnectors = Option.filter_and_unpack (
     fun ((i,j),result)->
      if (result <> (Hex_cell_set.empty_set,[])) && 
      (List.mem pr_idx [i;j]) 
      then Some(i,j)
      else None
   ) draft in
   if (List.length relevant_coconnectors) < 3 
   then relevant_coconnectors 
   else [] ;; 

let pattern1 = Hex_pattern_t.Pat (
  [    
                  (1,2),true ;(1,3),false;
      (2,1),false;(2,2),false;(2,3),true ;
      (3,1),true ;(3,2),false
   ]
);;

let deduce_removabilities
    end_of_battle ctct_report draft =
    let occurrences = Hex_pattern.occurrences_of_in pattern1 end_of_battle in 
    List.flatten (Image.image (deduce_removabilities_from_pattern ctct_report draft) occurrences);; 

let second_draft_from_previous_items eob base ctct_report = 
   let draft1 = first_draft_from_previous_items eob base ctct_report in 
   let removabilities = deduce_removabilities eob ctct_report draft1 in 
   let (Hex_ccnn_report_t.R l_draft1) = draft1 in 
   let l_draft2 = List.filter (
      fun (key,answer) ->
         (not (List.mem key removabilities)) && (answer<>(Hex_cell_set.empty_set,[])) 
   ) l_draft1 in 
   Hex_ccnn_report_t.R l_draft2;;

end ;; 

let draft_from_previous_items = Private.second_draft_from_previous_items ;; 