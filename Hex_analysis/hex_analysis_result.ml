(* 

#use"Hex_analysis/hex_analysis_result.ml";;

*)


exception No_moves_to_choose_from;;

let first_from_set (Ordered.S(l))=match l with 
  []->None
  |first::others->Some(first);;

let usual_move  res =
  let opt1=first_from_set res.Hex_analysis_result_t.winning_moves in 
  if opt1<>None then Option.unpack opt1 else 
  let opt2=first_from_set res.Hex_analysis_result_t.already_used_moves in 
  if opt2<>None then Option.unpack opt2 else 
  raise(No_moves_to_choose_from);;  


  
  