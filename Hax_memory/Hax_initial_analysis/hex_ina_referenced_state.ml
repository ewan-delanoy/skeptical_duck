(* 

#use"Hex_memory/Hex_initial_analysis/hex_ina_referenced_state.ml";;

*)


let initial_state = Hax_ina_state_t.Foreseen_so_far
                      (Hex_checked_initial_game.empty_one);;


let one_move_more mmrzr cell old_state= 
   Hex_ina_state.one_move_more (!(Hex_persistent_ina_memorizer.main)) cell old_state;;


let declare_winner winner final_state= 
   Hex_ina_state.one_move_more (!(Hex_persistent_ina_memorizer.main)) winner final_state;;
        