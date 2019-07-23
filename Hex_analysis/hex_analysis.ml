(* 

#use"Hex_analysis/hex_analysis.ml";;

*)

let initial_point ()= 
  let ista = Hex_state.initial_state () in 
   (
      ista ,
      Hex_state.analize ista
   );;


let walker=ref(initial_point());;

let restart () =(walker := initial_point());;

let absorb_move cell=
   let (old_state,_)=(!walker) in 
   let new_state = Hex_state.absorb_move old_state cell in 
   let new_result = Hex_state.analize new_state in 
   let _=(walker:=(new_state,new_result)) in 
   new_result;;

let usual_move ()=
  let cell = Hex_analysis_result.usual_move (snd(!walker)) in 
  absorb_move cell;;


let declare_winner player =
  let new_fgame={
    Hex_finished_game_t.winner = player ;
    Hex_finished_game_t.sequence_of_moves = List.rev((fst(!walker)).Hex_state_t.moves_before)
  } in 
  Hex_persistent.add_finished_game new_fgame;;

  