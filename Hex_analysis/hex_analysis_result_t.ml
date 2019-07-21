(* 

#use"Hex_analysis/hex_analysis_result_t.ml";;

*)


type t= {
   mandatory_set : Hex_cell_t.t Ordered.old_set option;
   winning_moves : Hex_cell_t.t Ordered.old_set;
   already_used_moves : Hex_cell_t.t Ordered.old_set;
};;

  
  