(* 

#use"Hex_memory/Hex_initial_analysis/hex_ina_state.ml";;

*)


let initial_empty_state = Hax_ina_state_t.Foreseen_so_far(Hax_checked_initial_game.empty_one);;

let pgame_from_state sta=match sta with 
    Hax_ina_state_t.Foreseen_so_far(pgame)->pgame
   |Hax_ina_state_t.Awaiting_final_outcome(pgame)->pgame;;

let analize_game mmrzr pgame =
   let forecasts = Hax_ina_memorizer.cut_by mmrzr pgame in 
   Hax_cigame_collection.classify_according_to_depth forecasts;;

let consult mmrzr sta=analize_game mmrzr (pgame_from_state sta);;

let one_move_more mmrzr cell old_state= match old_state with 
    Hax_ina_state_t.Awaiting_final_outcome(_)->(old_state,[])
    |Hax_ina_state_t.Foreseen_so_far(pgame)->
      let new_pgame = Hax_checked_initial_game.one_move_more pgame cell in 
      let new_state=(
      if Hax_ina_memorizer.is_foreseen_in new_pgame mmrzr
      then Hax_ina_state_t.Foreseen_so_far(new_pgame)
      else Hax_ina_state_t.Awaiting_final_outcome(new_pgame)
      ) in 
      let forecasts = Hax_ina_memorizer.cut_by mmrzr new_pgame in 
      let offers = Hax_cigame_collection.classify_according_to_depth forecasts in 
      (new_state,offers);;

let declare_winner winner final_state= 
  match final_state with 
    |Hax_ina_state_t.Foreseen_so_far(_)->None
    |Hax_ina_state_t.Awaiting_final_outcome(pgame)->
      let wanted_winner = Hax_checked_initial_game.last_one_to_play pgame in 
      if winner <> wanted_winner
      then None
      else Some(pgame);;
        