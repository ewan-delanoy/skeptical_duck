(* 

#use"Hex_analysis/hex_analysis_result_t.ml";;

*)


type t= {
   next_to_play : Hex_player_t.t ; 
   mandatory_set : Hex_cell_set_t.t option;
   involved_end_strategies : int list;
   easy_advancer : (Hex_cell_t.t * (Hex_cell_t.t list)) option;
   strong_moves : Hex_cell_set_t.t ;
   usual_move : Hex_cell_t.t ; 
   number_of_remaining_enemies : int ;
};;

  
  