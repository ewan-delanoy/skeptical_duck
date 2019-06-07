(* 

#use"Hex_memory/hex_analysis.ml";;

First coordinate is column index, second is row index

*)

let memorizer = ref(Hex_pgame_memorizer.initial_one (Hex_cell.of_string "f6"));;
let state = ref(Hex_analysis_state_t.Foreseen_so_far(Hex_partial_game.empty_one));;

exception Your_move_was_ignored;;

let one_move_more cell = match (!state) with 
    Hex_analysis_state_t.Awaiting_final_outcome(_)->raise(Your_move_was_ignored)
    |Hex_analysis_state_t.Foreseen_so_far(pgame)->
      let new_pgame = Hex_partial_game.one_move_more pgame cell in 
      let new_state=(
      if Hex_pgame_memorizer.is_foreseen_in new_pgame (!memorizer)
      then Hex_analysis_state_t.Foreseen_so_far(new_pgame)
      else Hex_analysis_state_t.Awaiting_final_outcome(new_pgame)
      ) in 
      let _=(state:=new_state) in 
      (new_state,new_pgame);;
   
exception Result_not_helpful ;;
exception Forecast_not_finished;;

let finalize winner = 
  match (!state) with 
    |Hex_analysis_state_t.Foreseen_so_far(_)->raise(Forecast_not_finished)
    |Hex_analysis_state_t.Awaiting_final_outcome(pgame)->
      let wanted_winner = Hex_partial_game.last_one_to_play pgame in 
      if winner <> wanted_winner
      then raise(Result_not_helpful)
      else 
        let new_mmrzr = Hex_pgame_memorizer.insert_in pgame (!memorizer)  in 
        let _=(memorizer:=new_mmrzr;
               state:=Hex_analysis_state_t.Foreseen_so_far(Hex_partial_game.empty_one);
               Hex_pgame_memorizer.remember_as_example new_mmrzr) in 
        new_mmrzr;; 

(* Shortcuts and sugar *)

exception Unknown_player_index of int ;;

let player_at_index = function 
   1->Hex_player_t.First_player
   |2->Hex_player_t.Second_player
   |k->raise(Unknown_player_index(k));;

let omm s=one_move_more (Hex_cell.of_string s);;
let fz k=finalize(player_at_index k);;















