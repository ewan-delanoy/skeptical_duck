(* 

#use"Hex_analysis/hex_ccnn_graph.ml";;

*) 

let neighbors (Hex_ccnn_report_t.R l) i =
   Option.filter_and_unpack (
      fun ((j,k),data) ->
        if j=i then Some(k,data) else 
        if k=i then Some(j,data) else None
   ) l ;;
  