(* 

#use"Hex_analysis/hex_analysis.ml";;

*)

let current_dim = 11;;

let empty_point = 
  let ista = Hex_state.empty_state  in 
   (
      ista ,
      Hex_state.analize ista
   );;



let walker=ref(empty_point);;

let initial_point my_name= 
  let ista = Hex_state.initial_state my_name in 
   (
      ista ,
      Hex_state.analize ista
   );;

let restart my_name =let _=(walker := initial_point my_name) in snd(!walker);;

let absorb_move cell=
   let (old_state,_)=(!walker) in 
   let new_state = Hex_state.absorb_move old_state cell in 
   let new_result = Hex_state.analize new_state in 
   let _=(walker:=(new_state,new_result)) in 
   new_result;;

let absorb_moves cells=
   let _=List.iter (fun cell->let _=absorb_move cell in ()) cells in 
   snd(!walker);; 

(*
let usual_move ()=
  let cell = Hex_analysis_result.usual_move (snd(!walker)) in 
  absorb_move cell;;
*)

let remember_opening_if_necessary winner =
   if winner = (fst(!walker)).Hex_state_t.whoami 
   then ()
   else 
   let (opt,strong_moves_before)=(fst(!walker)).Hex_state_t.strong_moves_before in 
   (* It may happen that there is no new opening to remember if the
   "whoami" player has not obeyed the rules *)
   if Hex_common.next_one_to_play(strong_moves_before) <> winner 
   then ()
   else let new_l=List.rev((Option.unpack opt)::strong_moves_before) in 
        let new_opng = Hex_strong_opening_t.O(new_l) in 
        Hex_persistent.add_strong_opening new_opng;;


let declare_winner player =
  let new_fgame={
    Hex_finished_game_t.dimension = current_dim;
    Hex_finished_game_t.winner = player ;
    Hex_finished_game_t.sequence_of_moves = List.rev((fst(!walker)).Hex_state_t.moves_before)
  } in 
  let _=Hex_persistent.add_finished_game new_fgame in
  let _=remember_opening_if_necessary player in  
  let new_grid = Hex_ascii_grid.of_finished_game new_fgame in 
  Hex_ascii_grid.print_on_sheet_for_editing new_grid;;

  