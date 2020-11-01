(* 

#use"Hex_analysis/hex_maximal_exact_paths.ml";;

*) 

let compute_maximal_exact_paths ucr =
   Maximal_paths_in_acyclic_graph.maximal_paths 1 
    (ucr.Hex_uc_report_t.connectors) ;;
      