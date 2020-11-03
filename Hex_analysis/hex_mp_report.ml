(* 

#use"Hex_analysis/hex_mp_report.ml";;

*)

module Private = struct 
let one_step_constructor uc_report  = 
   let l_connectors = uc_report.Hex_uc_report_t.connectors in 
   let mp = Maximal_paths_in_acyclic_graph.maximal_paths l_connectors in 
{
   Hex_mp_report_t. dimension = uc_report.Hex_uc_report_t.dimension ;
                       winner = uc_report.Hex_uc_report_t.winner ;
              enemy_territory = uc_report.Hex_uc_report_t.enemy_territory;
                       items  = uc_report.Hex_uc_report_t.items;
                       base   = uc_report.Hex_uc_report_t.base ;
                   connectors = l_connectors ;
                 paths_from_1 = mp 1 ;   
                 paths_from_2 = mp 2 ;   

} ;;

let irregularities l= 
   let temp1 = List.flatten (Image.image snd l) in 
   let temp2 = Image.image fst temp1 in 
   let temp3 = Ordered.sort Total_ordering.standard temp2 in 
   let temp4 = Uple.list_of_pairs temp3 in 
   Option.filter_and_unpack (
     fun (uc1,uc2) ->
        let z = Hex_cell_set.intersect 
          (Hex_unified_connector.support uc1) 
          (Hex_unified_connector.support uc2)  in 
        if Hex_cell_set.length(z)>0 
        then Some(uc1,uc2,z)
        else None   
   ) temp4 ;;


end ;; 


let cumulative_constructor fg =
   let ctct_report = Hex_ctct_report.constructor fg in 
   let uc_report = Hex_uc_report.one_step_constructor ctct_report in 
   Private.one_step_constructor uc_report  ;;  

let irregularities mp_report =
    (
      Private.irregularities mp_report.Hex_mp_report_t.paths_from_1,
      Private.irregularities mp_report.Hex_mp_report_t.paths_from_2
    );;

let one_step_constructor = Private.one_step_constructor ;;


