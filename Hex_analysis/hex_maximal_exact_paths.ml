(* 

#use"Hex_analysis/hex_maximal_exact_paths.ml";;

*) 

let compute_maximal_exact_paths ucr =
   Maximal_paths_in_acyclic_graph.maximal_paths  
    (ucr.Hex_uc_report_t.connectors) 1;;
      