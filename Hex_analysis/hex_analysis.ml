(* 

#use"Hex_analysis/hex_analysis.ml";;

*)

let current_dim = 11;;

let initial_point ()= 
  let ista = Hex_state.initial_state () in 
   (
      ista ,
      Hex_state.analize ista
   );;

let walker=ref(Hex_state.empty_state);;

let restart () =(walker := initial_point());;

let absorb_move cell=
   let (old_state,_)=(!walker) in 
   let new_state = Hex_state.absorb_move old_state cell in 
   let new_result = Hex_state.analize new_state in 
   let _=(walker:=(new_state,new_result)) in 
   new_result;;

let absorb_moves cells=
   let _=List.iter (fun cell->let _=absorb_move cell in ()) cells in 
   snd(!walker);; 

let usual_move ()=
  let cell = Hex_analysis_result.usual_move (snd(!walker)) in 
  absorb_move cell;;


let declare_winner player =
  let new_fgame={
    Hex_finished_game_t.dimension = current_dim;
    Hex_finished_game_t.winner = player ;
    Hex_finished_game_t.sequence_of_moves = List.rev((fst(!walker)).Hex_state_t.moves_before)
  } in 
  let _=Hex_persistent.add_finished_game new_fgame in 
  let new_grid = Hex_ascii_grid.of_finished_game new_fgame in 
  Hex_ascii_grid.print_on_sheet_for_editing new_grid;;

  