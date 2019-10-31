(* 

#use"Hex_analysis/hex_analysis.ml";;

*)

let walker=ref(Hex_state.empty_state,Hex_analysis_result.empty_result);;
let preceding_position=ref(Hex_state.empty_state,Hex_analysis_result.empty_result);;

let initial_point ()= 
  let ista = Hex_state.initial_state () in 
   (
      ista ,
      Hex_state.analize ista
   );;

exception No_latest_winner;;   

let latest_winner = ref(None);;

let get_latest_winner () = match (!latest_winner) with 
   None->raise(No_latest_winner)
   |Some(player)->player;;


let restart () =let _=(walker := initial_point ();latest_winner:=None) in snd(!walker);;

let absorb_move cell=
   let (old_state,old_res)=(!walker) in
   let _=(preceding_position:=(old_state,old_res)) in 
   let new_state = Hex_state.absorb_move old_state cell in 
   let new_result = Hex_state.analize new_state in 
   let _=(walker:=(new_state,new_result)) in 
   new_result;;

let undo_last_absorption ()=
    let (old_state,old_res)=(!preceding_position) in
    let _=(walker:=(old_state,old_res)) in 
    old_res;;


let absorb_all_moves cells=
   let _=List.iter (fun cell->let _=absorb_move cell in ()) cells in 
   snd(!walker);; 

let absorb_some_moves cells j=absorb_all_moves (Listennou.big_head j cells);;


let remember_opening_if_necessary winner status =
   if status = Hex_status_t.Winner 
   then (* If I won, there is no difficult opening to remember *)
        ()
   else 
   let (opt,strong_moves_before)=(fst(!walker)).Hex_state_t.strong_moves_before in 
   if Hex_common.next_one_to_play(strong_moves_before) <> winner 
   then (* in this case, the first non-strong was played by me and was a mistake, and is
           not to be remembered *)
        ()
   else let new_l=List.rev((Option.unpack opt)::strong_moves_before) in 
        let new_opng = Hex_strong_opening_t.O(new_l) in 
        Hex_persistent.add_strong_opening new_opng;;

let declare_winner player status=
  let _=(latest_winner:=Some(player)) in 
  let new_fgame={
    Hex_finished_game_t.dimension = Hex_persistent.dimension();
    Hex_finished_game_t.winner = player ;
    Hex_finished_game_t.sequence_of_moves = List.rev((fst(!walker)).Hex_state_t.moves_before)
  } in 
  let _=Hex_persistent.add_finished_game new_fgame in
  let _=remember_opening_if_necessary player status in  
  let new_grid = Hex_ascii_grid.of_finished_game new_fgame in 
  Hex_ascii_grid.print_on_sheet_for_editing new_grid;;


let add_basic_linker comment=
   let winner = get_latest_winner () 
   and grid = Hex_ascii_grid.process_sheet () in 
   let (a,p) = Hex_ascii_grid.to_basic_linker grid in 
   let linker = Hex_strategy_static_constructor_t.Basic_Linker(a,p) in
   Hex_persistent.add_end_strategy
   (winner,linker,comment,[]);;


let replay_and_declare_winner ()=
  let (role_played,cells) = Hex_parse_playok_format.parse () in 
  let winner = Hex_common.has_just_played cells in 
  let status = (
      if winner = role_played 
      then Hex_status_t.Winner 
      else Hex_status_t.Loser
  ) in 
  let _=restart () in 
  let _=absorb_all_moves cells in 
  declare_winner winner status;;

let move_as_usual () =
   let cell = (snd(!walker)).Hex_analysis_result_t.usual_move in 
   absorb_move cell;; 