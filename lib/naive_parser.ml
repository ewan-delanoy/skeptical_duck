(*

#use"lib/naive_parser.ml";;

*)

module Private = struct 

let try_parse_at_index (Naive_parser_t.NP f) text idx = f text idx ;;

let try_parse_after_index np text initial_idx= 
  let n = String.length text in 
  let rec helper=(fun current_idx -> 
  if current_idx > n 
  then None  
  else 
  match try_parse_at_index np text current_idx with 
  (Some success) -> Some(current_idx,success)
  |None ->  helper (current_idx+1)
  ) in 
  helper initial_idx ;;

let inner_concat nps text idx = 
   let rec helper = (
     fun (treated,remaining_nps,current_idx) -> match remaining_nps with 
     [] -> Some(List.rev treated,current_idx)
     |np::other_nps ->
       match try_parse_at_index np text idx with 
       None -> None 
       |Some (part,new_idx) -> helper (part::treated,other_nps,new_idx)
   ) in 
   helper ([],nps,idx) ;;

let concat nps = Naive_parser_t.NP(inner_concat nps);;

let inner_concat_two_then_backtrack np1 np2 text idx = 
  match try_parse_at_index np1 text idx with 
  None -> None 
  |Some (part1,idx1) ->
   (
     match try_parse_at_index np2 text idx1 with 
      None -> None 
     |Some (_,_) -> Some(part1,idx1)
   );;

let concat_two_then_backtrack np1 np2 = Naive_parser_t.NP(inner_concat_two_then_backtrack np1 np2);;

let inner_disjunction nps text idx = 
   let rec helper = (
     fun remaining_nps -> match remaining_nps with 
     [] -> None
     |np::other_nps ->
      let opt = try_parse_at_index np text idx in 
      if opt<>None then opt else 
      helper other_nps
   ) in 
   helper nps ;;

let disjunction nps = Naive_parser_t.NP(inner_disjunction nps);;

let inner_concat_star_with_nonstar_then_backtrack np_in_star np_after_star text idx = 
  let rec helper = (
    fun (treated,current_idx) -> 
     match try_parse_at_index np_after_star text current_idx with 
     Some (_,_) ->  if treated=[] then None else Some(List.rev treated,current_idx)
     |None ->  
      match try_parse_at_index np_in_star text current_idx with 
       None -> if treated=[] then None else Some(List.rev treated,current_idx) 
       |Some(part,new_idx) -> helper (part::treated,new_idx)
   ) in 
   helper ([],idx);;

let concat_star_with_nonstar_then_backtrack np1 np2 = Naive_parser_t.NP(inner_concat_star_with_nonstar_then_backtrack np1 np2);;



end ;;  

let concat_star_with_nonstar_then_backtrack = Private.concat_star_with_nonstar_then_backtrack ;;

let concat_two_then_backtrack = Private.concat_two_then_backtrack ;;

let concat = Private.concat ;;

let disjunction = Private.disjunction ;; 

let try_parse_after_index = Private.try_parse_after_index ;;

let try_parse_at_index = Private.try_parse_at_index ;;
   