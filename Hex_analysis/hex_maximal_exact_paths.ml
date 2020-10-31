(* 

#use"Hex_analysis/hex_maximal_exact_paths.ml";;

*) 

let neighbors  idx connectors= 
   let temp1 = Option.filter_and_unpack (
      fun ((i,j),connector)->
          if i=idx then Some(j,connector) else 
          if j=idx then Some(i,connector) else
          None     
   ) connectors in 
   Ordered.sort Total_ordering.standard2 temp1 ;;

let extensions (joins,starter,yet_unused_connectors) =


