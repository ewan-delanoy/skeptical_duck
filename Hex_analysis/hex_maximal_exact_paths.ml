(* 

#use"Hex_analysis/hex_maximal_exact_paths.ml";;

*) 

(*
type connector_type = string ;;

type base_type = ((int * int) * connector_type) list ;;

let order_for_connectors = (Total_ordering.standard : connector_type Total_ordering.t);;

let order_for_int_pairs = Total_ordering.product Total_ordering.for_integers Total_ordering.for_integers;;

let order_for_base_elements = Total_ordering.product order_for_int_pairs order_for_connectors;;

let order_for_bases = Total_ordering.lex_compare order_for_base_elements;;

let order_for_results = Total_ordering.product order_for_connectors order_for_bases;;

let rec helper_for_neighbors ord new_case (idx,established,tested,yet_untested) =
     match yet_untested with 
     []->(Ordered.sort ord (* order_for_results *) established )
     |next_one :: others ->
      let  ((i,j),connector) = next_one in 
      if (idx<>i)&&(idx<>j)
      then  let smaller_base = List.rev_append tested others in 
            let other_idx= (if idx=i then j else i) in
            (* let new_case =(other_idx,(connector,smaller_base)) in  *)
            helper_for_neighbors ord new_case (idx,new_case::established,next_one::tested,others)
      else helper_for_neighbors ord new_case (idx,established,next_one::tested,others) ;;      

let neighbors idx connectors = helper_for_neighbors (idx,[],[],connectors);;


let extensions (joins,starter,yet_unused_connectors) =
     let last_used_idx = (match joins with 
     []->starter 
     |(idx,join)::_->idx
     ) in 
    let temp1 = neighbors last_used_idx  yet_unused_connectors in 
    if temp1 = [] 
    then  (* we have found a maximal extension *)
         (Some(joins,starter),None)
    else            



      let rec helper_for_neighbors ord (idx,established,tested,yet_untested,nc) =
         match yet_untested with 
         []->(Ordered.sort ord (* order_for_results *) established )
         |next_one :: others ->
           ( 
          let  ((i,j),connector) = next_one in 
          if (idx<>i)&&(idx<>j)
          then (let smaller_base = List.rev_append tested others in 
                let other_idx= (if idx=i then j else i) in
                let now_case =(other_idx,(connector,smaller_base)) in  
                helper_for_neighbors ord (idx,now_case::established,next_one::tested,others,now_case)
          ) 
          else helper_for_neighbors ord (idx,established,next_one::tested,others,nc) 
           );;

*)