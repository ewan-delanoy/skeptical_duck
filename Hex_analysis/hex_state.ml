(* 

#use"Hex_analysis/hex_state.ml";;

*)

let empty_state = 
{
   Hex_state_t.config_remains = Hex_fles_double_list.empty_one ;
   Hex_state_t.games_remains = Hex_fg_double_list.empty_one ;
   Hex_state_t.openings_remains = [] ;
   Hex_state_t.moves_before = [] ;
   Hex_state_t.declared_participant = None ;
};;

let initial_state opt_participant= 
   let _=Hex_persistent.initialize_all_data_if_necessary () in 
{
   Hex_state_t.config_remains = ((Hex_end_strategy_factory.compute_all_end_configs Hex_persistent.wes_pair)) ;
   Hex_state_t.games_remains = (!(Hex_persistent.games_ref)) ;
   Hex_state_t.openings_remains = (!(Hex_persistent.untamed_openings_ref));
   Hex_state_t.moves_before = [];
   Hex_state_t.declared_participant = opt_participant ;
};;

exception No_moves_to_choose_from;;

let compute_chosen_move (dim,strong_moves_data,fam_moves,condition,moves_before) =
  let opt1=strong_moves_data in 
  if opt1<>None then (fun (_,move,_)->move)(Option.unpack opt1) else 
  let opt2=Hex_cell_set.optional_min(fam_moves) in 
  if opt2<>None then Option.unpack opt2 else 
  let remaining_world = Hex_cell_set.apply_condition condition (Hex_common.all_cells dim) in
  let free_cells=Hex_cell_set.setminus remaining_world (Hex_cell_set.safe_set moves_before) in 
  let opt3=Hex_cell_set.optional_min(free_cells) in 
  if opt3<>None then Option.unpack opt3 else 
  raise(No_moves_to_choose_from);;  


let default_dim = Hex_dimension_t.D 11 ;;
let current_dim sta = try Hex_fles_double_list.dimension sta.Hex_state_t.config_remains with 
  _ -> default_dim ;;


let analize sta=
  let dim = current_dim sta
  and player = Hex_common.next_one_to_play (sta.Hex_state_t.moves_before) in 
  let (cells,enemy_strats,mand)=Hex_fles_double_list.immediate_dangers player sta.Hex_state_t.config_remains in
  let condition = Hex_mandatory_compound.global_escape_set mand in 
  let strong_moves_data = Hex_uog_list.seek_interesting_move sta.Hex_state_t.openings_remains in
  let fam_moves = Hex_cell_set.apply_condition condition (Hex_fg_double_list.familiar_moves player sta.Hex_state_t.games_remains) in 
  let u_move = compute_chosen_move (dim,strong_moves_data,fam_moves,condition,sta.Hex_state_t.moves_before) in 
  let n_enem = Hex_fles_double_list.number_of_enemy_strategies player sta.Hex_state_t.config_remains in 
  let info_requested =(
     match sta.Hex_state_t.declared_participant with 
     None -> true 
     |Some(declared)->player = declared
  ) in 

  {
     Hex_analysis_result_t.next_to_play = player ; 
     mandatory_compound = mand ;
     dangerous_enemy_strategies = enemy_strats ;
     completion_for_strong_move = Option.propagate (fun (forcing,_,x)->(forcing,x) ) strong_moves_data ;
     familiar_moves = fam_moves;
     chosen_move = u_move;
     number_of_remaining_enemies = n_enem;
     info_needed = info_requested;
  } ;;

let absorb_move sta cell=
   let player = Hex_common.next_one_to_play (sta.Hex_state_t.moves_before) in 
   {
      sta with 
      Hex_state_t.config_remains = Hex_fles_double_list.simplify_by_move (player,cell) sta.Hex_state_t.config_remains ;
      Hex_state_t.games_remains = Hex_fg_double_list.simplify_by_move cell sta.Hex_state_t.games_remains ;
      Hex_state_t.openings_remains = Hex_uog_list.simplify_by_move cell sta.Hex_state_t.openings_remains ;
      Hex_state_t.moves_before =  (cell::(sta.Hex_state_t.moves_before)) ;
   };;

let finish_game sta={
    Hex_finished_game_t.dimension = current_dim sta;
    Hex_finished_game_t.winner = Hex_common.has_just_played(sta.Hex_state_t.moves_before) ;
    Hex_finished_game_t.sequence_of_moves = List.rev(sta.Hex_state_t.moves_before)
  } ;;