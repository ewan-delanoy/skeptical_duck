(* 

#use"Hex_analysis/hex_analysis.ml";;

*)



let walker=ref(Hex_state.empty_state,Hex_analysis_result.empty_result);;
let preceding_position=ref(Hex_state.empty_state,Hex_analysis_result.empty_result);;

let initial_point opt_participant= 
  let ista = Hex_state.initial_state opt_participant in 
   (
      ista ,
      Hex_state.analize ista
   );;

exception No_latest_winner;;   

let latest_winner = ref(None);;

let get_latest_winner () = match (!latest_winner) with 
   None->raise(No_latest_winner)
   |Some(player)->player;;


let restart opt_participant =let _=(walker := initial_point opt_participant;latest_winner:=None) in snd(!walker);;

let deal_with_critical_accumulation ()=
    let sta=fst(!walker) in 
    let player = Hex_common.next_one_to_play (sta.Hex_state_t.moves_before) in 
    let (cells,enemy_indices,mand)=Hex_fles_double_list.immediate_dangers player sta.Hex_state_t.config_remains in 
    if not(Hex_mandatory_compound.test_for_unrealizable_constraint mand) 
    then ()
    else 
    let fgame = Hex_state.finish_game sta in 
    let _=Hex_persistent.add_finished_game fgame in 
    let constructor = Hex_strategy_static_constructor_t.Exhaustive_Disjunction(cells) in 
    let _ = Hex_persistent.add_end_strategy
   (Hex_player.other_player player,constructor,"disjunction from ",enemy_indices) in 
   ();;



exception Absorb_move_exn of string;;

let absorb_move cell=
   if (not(!(Hex_persistent.data_has_been_initialized_already)))
   then raise(Absorb_move_exn("call Hex_analysis.restart before playing a game"))
   else 
   let (old_state,old_res)=(!walker) in
   let _=(preceding_position:=(old_state,old_res)) in 
   let new_state = Hex_state.absorb_move old_state cell in 
   let new_result = Hex_state.analize new_state in 
   let _=(walker:=(new_state,new_result)) in 
   let _=(
       if Hex_mandatory_compound.test_for_unrealizable_constraint 
            (new_result.Hex_analysis_result_t.mandatory_compound)
       then deal_with_critical_accumulation ()     
   ) in
   new_result;;

let undo_last_absorption ()=
    let (old_state,old_res)=(!preceding_position) in
    let _=(walker:=(old_state,old_res)) in 
    old_res;;


let absorb_all_moves cells=
   let _=List.iter (fun cell->let _=absorb_move cell in ()) cells in 
   snd(!walker);; 

let absorb_some_moves cells j=absorb_all_moves (Listennou.big_head j cells);;



let declare_winner player =
  let _=(latest_winner:=Some(player)) in 
  let pre_new_fgame=Hex_state.finish_game (fst(!walker)) in 
  let new_fgame= { pre_new_fgame with 
    Hex_finished_game_t.winner = player ;
  } in 
  let _=Hex_persistent.add_finished_game new_fgame in
  let new_grid = Hex_ascii_grid.of_finished_game new_fgame in 
  Hex_ascii_grid.print_on_sheet_for_editing new_grid;;


let add_molecular_linker comment=
   let winner = get_latest_winner () 
   and grid = Hex_ascii_grid.process_sheet () in 
   let (mlclr,actives) = Hex_ascii_grid.to_molecular_linker_with_active_points grid in 
   let linker = Hex_strategy_static_constructor_t.Molecular(mlclr,actives) in
   Hex_persistent.add_end_strategy
   (winner,linker,comment,[]);;


let replay_and_declare_winner ()=
  let (_,cells) = Hex_parse_playok_format.parse () in 
  let winner = Hex_common.has_just_played cells in 
  let _=restart None in 
  let _=absorb_all_moves cells in 
  declare_winner winner;;

let move_as_usual () =
   let cell = (snd(!walker)).Hex_analysis_result_t.chosen_move in 
   absorb_move cell;; 
