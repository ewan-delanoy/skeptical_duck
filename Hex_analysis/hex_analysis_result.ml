(* 

#use"Hex_analysis/hex_analysis_result.ml";;

*)

let empty_result = 
{
     Hex_analysis_result_t.mandatory_set = Hex_cell_set_t.S [];
     involved_end_strategies = [] ;
     easy_advances = [] ;
     strong_moves =  Hex_cell_set_t.S [];
     already_used_moves = Hex_cell_set_t.S [] ;
     usual_move = Hex_cell.of_string "a1"; (* arbitrary, will never be used *)
  } 


  
  