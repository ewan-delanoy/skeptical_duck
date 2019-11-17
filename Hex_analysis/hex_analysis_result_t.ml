(* 

#use"Hex_analysis/hex_analysis_result_t.ml";;

*)


type t= {
   next_to_play : Hex_player_t.t ; 
   mandatory_set : Hex_cell_set_t.t option;
   dangerous_enemy_strategies : int list;
   strong_move : (Hex_cell_t.t * (Hex_cell_t.t list)) option;
   familiar_moves : Hex_cell_set_t.t ;
   chosen_move : Hex_cell_t.t ; 
   number_of_remaining_enemies : int ;
   declared_partipant : Hex_player_t.t option;
   info_needed : bool;
};;

  
  