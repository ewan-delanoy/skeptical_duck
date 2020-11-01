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

end ;; 


let cumulative_constructor fg =
   let ctct_report = Hex_ctct_report.constructor fg in 
   let uc_report = Hex_uc_report.one_step_constructor ctct_report in 
   Private.one_step_constructor uc_report  ;;  

let one_step_constructor = Private.one_step_constructor ;;
