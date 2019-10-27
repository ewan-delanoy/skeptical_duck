(* 

#use"Hex_analysis/hex_analysis_result_t.ml";;

*)


type t= {
   mandatory_set : Hex_cell_set_t.t option;
   involved_end_strategies : int list;
   easy_advances : Hex_cell_set_t.t ;
   strong_moves : Hex_cell_set_t.t ;
   already_used_moves : Hex_cell_set_t.t;
   usual_move : Hex_cell_t.t ; 
   sizes : int * int * int * int * int ;
};;

  
  