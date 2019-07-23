(* 

#use"Hex_analysis/hex_analysis_result.ml";;

*)


exception No_moves_to_choose_from;;

let usual_move  res =
  let opt1=Hex_cell_set.optional_min(res.Hex_analysis_result_t.winning_moves) in 
  if opt1<>None then Option.unpack opt1 else 
  let opt2=Hex_cell_set.optional_min(res.Hex_analysis_result_t.already_used_moves) in 
  if opt2<>None then Option.unpack opt2 else 
  raise(No_moves_to_choose_from);;  


  
  