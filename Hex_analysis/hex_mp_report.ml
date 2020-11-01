(* 

#use"Hex_analysis/hex_mp_report.ml";;

*)

let from_previous_items uc_report  = 
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