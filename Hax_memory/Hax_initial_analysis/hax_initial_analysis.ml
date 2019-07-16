(* 

#use"Hex_memory/hex_initial_analysis.ml";;

*)

let memorizer = ref(Hax_ina_memorizer.initial_one (Hax_cell.of_string "f6"));;
let state = ref(Hax_ina_state_t.Foreseen_so_far(Hax_checked_initial_game.empty_one));;

exception Your_move_was_ignored;;

let current_pgame ()=match (!state) with 
    Hax_ina_state_t.Foreseen_so_far(pgame)->pgame
   |Hax_ina_state_t.Awaiting_final_outcome(pgame)->pgame;;

let analize_game pgame =
   let forecasts = Hax_ina_memorizer.cut_by (!memorizer) pgame in 
   Hax_cigame_collection.classify_according_to_depth forecasts;;

let consult ()=analize_game (current_pgame());;

let one_move_more cell = match (!state) with 
    Hax_ina_state_t.Awaiting_final_outcome(_)->raise(Your_move_was_ignored)
    |Hax_ina_state_t.Foreseen_so_far(pgame)->
      let new_pgame = Hax_checked_initial_game.one_move_more pgame cell in 
      let new_state=(
      if Hax_ina_memorizer.is_foreseen_in new_pgame (!memorizer)
      then Hax_ina_state_t.Foreseen_so_far(new_pgame)
      else Hax_ina_state_t.Awaiting_final_outcome(new_pgame)
      ) in 
      let _=(state:=new_state) in 
      let forecasts = Hax_ina_memorizer.cut_by (!memorizer) new_pgame in 
      let offers = Hax_cigame_collection.classify_according_to_depth forecasts in 
      (offers,new_state,!memorizer);;

let suggested_move ()=snd(List.hd(consult()));;

let usual ()=one_move_more (suggested_move());;

let restart ()=(state:=Hax_ina_state_t.Foreseen_so_far(Hax_checked_initial_game.empty_one));;

exception Result_not_helpful ;;
exception Forecast_not_finished;;

let finalize winner = 
  match (!state) with 
    |Hax_ina_state_t.Foreseen_so_far(_)->raise(Forecast_not_finished)
    |Hax_ina_state_t.Awaiting_final_outcome(pgame)->
      let _=restart() in 
      let wanted_winner = Hax_checked_initial_game.last_one_to_play pgame in 
      if winner <> wanted_winner
      then raise(Result_not_helpful)
      else 
        let new_mmrzr = Hax_ina_memorizer.insert_in pgame (!memorizer)  in 
        let _=(memorizer:=new_mmrzr;
               Hax_ina_memorizer.remember_as_example new_mmrzr) in 
        new_mmrzr;; 

(* Shortcuts and sugar *)

exception Unknown_player_index of int ;;

let player_at_index = function 
   1->Hax_player_t.First_player
   |2->Hax_player_t.Second_player
   |k->raise(Unknown_player_index(k));;

let cs=consult;;
let u=usual;;
let omm s=one_move_more (Hax_cell.of_string s);;
let fz k=finalize(player_at_index k);;
let v n=let _=(for k=1 to n do let _=usual() in () done) in (consult(),!state,!memorizer);;
let rs=restart;;













