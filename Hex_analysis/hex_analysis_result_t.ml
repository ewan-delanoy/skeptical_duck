(* 

#use"Hex_analysis/hex_analysis_result_t.ml";;

*)


type t= {
   mandatory_set : Hex_cell_set_t.t option;
   winning_moves : Hex_cell_set_t.t ;
   already_used_moves : Hex_cell_set_t.t;
};;

  
  