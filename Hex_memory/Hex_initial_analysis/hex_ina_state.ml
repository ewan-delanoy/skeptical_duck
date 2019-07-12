(* 

#use"Hex_memory/Hex_initial_analysis/hex_ina_state.ml";;

*)



let memorizer = ref(Hex_cigame_memorizer.initial_one (Hex_cell.of_string "f6"));;

let initial_empty_state = Hex_ina_state_t.Foreseen_so_far(Hex_checked_initial_game.empty_one);;

let pgame_from_state sta=match sta with 
    Hex_ina_state_t.Foreseen_so_far(pgame)->pgame
   |Hex_ina_state_t.Awaiting_final_outcome(pgame)->pgame;;

let analize_game pgame =
   let forecasts = Hex_cigame_memorizer.cut_by (!memorizer) pgame in 
   Hex_cigame_collection.classify_according_to_depth forecasts;;

let consult sta=analize_game (pgame_from_state sta);;

let one_move_more cell old_state= match old_state with 
    Hex_ina_state_t.Awaiting_final_outcome(_)->old_state
    |Hex_ina_state_t.Foreseen_so_far(pgame)->
      let new_pgame = Hex_checked_initial_game.one_move_more pgame cell in 
      let new_state=(
      if Hex_cigame_memorizer.is_foreseen_in new_pgame (!memorizer)
      then Hex_ina_state_t.Foreseen_so_far(new_pgame)
      else Hex_ina_state_t.Awaiting_final_outcome(new_pgame)
      ) in 
      new_state;;

let declare_winner winner final_state= 
  match final_state with 
    |Hex_ina_state_t.Foreseen_so_far(_)->None
    |Hex_ina_state_t.Awaiting_final_outcome(pgame)->
      let wanted_winner = Hex_checked_initial_game.last_one_to_play pgame in 
      if winner <> wanted_winner
      then None
      else Some(pgame);;
        