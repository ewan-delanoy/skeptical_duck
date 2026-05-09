(*

#use"lib/Java_analysis/jvsp_parser.ml";;

*)

module Private = struct

let apply (Jvsp_types.Parser f) l idx= f l idx ;;

let helper_for_star prsr l =
    let rec helper = (
      fun (treated,idx,n) ->
        if idx>n then Some(List.rev treated,idx) else 
        match apply prsr l idx with 
        None ->  Some(List.rev treated,idx)
        | (Some (part,new_idx)) -> helper (part::treated,new_idx,n)
    ) in 
    helper ;;

let star prsr = Jvsp_types.Parser (fun tokens idx ->helper_for_star prsr tokens ([],idx,List.length tokens)) ;;

let map f prsr = Jvsp_types.Parser (fun tokens idx ->
  match (apply prsr tokens idx) with 
  None -> None 
  |(Some(data,new_idx)) -> Some(f data,new_idx)  
) ;;


(*
let helper_for_molecular (remaining_expectations,tokens,idx,n) = match remaining_expectations with 
  [] -> Some((),idx)
  |tok_type :: other_expectations -> 
      if idx>n then None else 
      let postok = List.nth tokens (idx-1) in 
      let tok_type2 = (postok.Jvsp_types.tok) in  
      
     

let helper_for_dis2 (Jvsp_types.Parser f1) (Jvsp_types.Parser f2) l =
    let rec helper = (
      fun (treated,idx,n) ->
        if idx>n then Some(List.rev treated,idx) else 
        match f l idx with 
        None ->  Some(List.rev treated,idx)
        | (Some (part,new_idx)) -> helper (part::treated,new_idx,n)
    ) in 
    helper ;;

*)    

end ;;


let map = Private.map ;;
let star = Private.star ;;
