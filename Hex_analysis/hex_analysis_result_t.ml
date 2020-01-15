(* 

#use"Hex_analysis/hex_analysis_result_t.ml";;

*)


type t= {
   next_to_play : Hex_player_t.t ; 
   mandatory_compound : Hex_mandatory_compound_t.t;
   dangerous_enemy_strategies : int list;
   completion_for_strong_move :  (bool * (Hex_cell_t.t list)) option;
   familiar_moves : Hex_cell_set_t.t ;
   chosen_move : Hex_cell_t.t ; 
   number_of_remaining_enemies : int ;
   info_needed : bool;
};;

  
  