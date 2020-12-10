(* 

#use"Hex_analysis/hex_mp_report.ml";;

*)

module Private = struct 

let maximal_paths_in_ag arg idx=
   let rewritten_arg = Image.image (
     fun ((Hex_ctct_index_t.I i,Hex_ctct_index_t.I j),x)->
      ((i,j),x)
   ) arg in 
   let pre_res = Maximal_paths_in_acyclic_graph.maximal_paths rewritten_arg idx in 
   Hex_mp_t.MP(
   Image.image (Image.image (fun (uc,k)->(uc,Hex_ctct_index_t.I k) )) pre_res);; 


let one_step_constructor uc_report  = 
   let l_connectors = uc_report.Hex_uc_report_t.connectors in 
   let mp = maximal_paths_in_ag l_connectors in 
   let paths_for_1 = mp 1 and paths_for_2 = mp 2 in 
   let old_ft = uc_report.Hex_uc_report_t.free_territory in 
   let used_cells = Hex_cell_set.merge 
      (Hex_mp.cells_from_junctions paths_for_1)
      (Hex_mp.cells_from_junctions paths_for_2) in 
{
   Hex_mp_report_t. dimension = uc_report.Hex_uc_report_t.dimension ;
                       winner = uc_report.Hex_uc_report_t.winner ;
               ally_territory = uc_report.Hex_uc_report_t.ally_territory;   
              enemy_territory = uc_report.Hex_uc_report_t.enemy_territory;
               free_territory = Hex_cell_set.setminus old_ft used_cells ;
                       items  = uc_report.Hex_uc_report_t.items;
                       base   = uc_report.Hex_uc_report_t.base ;
                   connectors = l_connectors ;
                 paths_from_1 = paths_for_1 ;   
                 paths_from_2 = paths_for_2 ;   

} ;;

let irregularities_from_pairs ll= 
   Option.filter_and_unpack (
     fun (uc1,uc2) ->
        let z = Hex_cell_set.intersect 
          (Hex_unified_connector.support uc1) 
          (Hex_unified_connector.support uc2)  in 
        if Hex_cell_set.length(z)>0 
        then Some(uc1,uc2,z)
        else None   
   ) ll ;;

let simplify_paths_presentation (Hex_mp_t.MP l)= 
   let temp1 = List.flatten l in 
   let temp2 = Image.image fst temp1 in 
   Ordered.sort Total_ordering.standard temp2 ;;   

let first_touches_second mp =
   let (Hex_mp_t.MP ll) = mp.Hex_mp_report_t.paths_from_1 in   
     List.exists 
     (fun l->List.exists (fun (_,y)->y=Hex_ctct_index_t.I 2) l)
     ll ;; 


let irregularities mp =
   let l1 = simplify_paths_presentation mp.Hex_mp_report_t.paths_from_1 
   and l2 = simplify_paths_presentation mp.Hex_mp_report_t.paths_from_2 in 
   let middle_part = (if 
        first_touches_second mp then [] else Cartesian.product l1 l2
      ) in 
   (
      irregularities_from_pairs(Uple.list_of_pairs l1),
      irregularities_from_pairs middle_part,
      irregularities_from_pairs(Uple.list_of_pairs l2)
   )
;;

(* to an allied cell, associate : 1 if it is joined to 1,
   2 if it is joined to 2, and 3 otherwise 
   *)
let classify_allied_cells mp =
     let joined_to_1 = Hex_mp.cells_from_components 
     mp.Hex_mp_report_t.items mp.Hex_mp_report_t.paths_from_1 
     and joined_to_2 = Hex_mp.cells_from_components 
     mp.Hex_mp_report_t.items mp.Hex_mp_report_t.paths_from_2 in 
     Hex_cell_set.image (
       fun cell ->
           let lbl=(
              if Hex_cell_set.mem cell joined_to_1 then 1 else 
              if Hex_cell_set.mem cell joined_to_2 then 2 else 3  
           ) in 
           (cell,lbl)    
     ) mp.Hex_mp_report_t.ally_territory;;

let first_pill_graph mp=
   let dim = mp.Hex_mp_report_t.dimension in 
   let classified_allies = classify_allied_cells mp in 
   let (Hex_cell_set_t.S free_cells) = mp.Hex_mp_report_t.free_territory in 
   let temp1 = Option.filter_and_unpack  (
     fun free_cell ->
       let ttemp2 = Hex_cell.neighbors dim free_cell in 
       let ttemp3 = Option.filter_and_unpack (
         fun cell -> try Some(cell,List.assoc cell classified_allies) with 
         _ -> None
       ) ttemp2 in 
       if ttemp3 = []
       then None 
      else Some(free_cell,ttemp3)  
   ) free_cells in 
   temp1;; 


end ;; 


let cumulative_constructor fg =
   let ctct_report = Hex_ctct_report.constructor fg in 
   let uc_report = Hex_uc_report.one_step_constructor ctct_report in 
   Private.one_step_constructor uc_report  ;;  

let irregularities = Private.irregularities ;; 

let one_step_constructor = Private.one_step_constructor ;;

 

   