(* 

#use"Hex_analysis/hex_state.ml";;

*)

let empty_state = 
{
   Hex_state_t.role_played = Hex_player_t.First_player; (* random decision, should never be used *)
   Hex_state_t.config_remains = Hex_fles_double_list.empty_one ;
   Hex_state_t.games_remains = Hex_fg_double_list.empty_one ;
   Hex_state_t.openings_remains = [] ;
   Hex_state_t.moves_before = [] ;
   Hex_state_t.strong_moves_before = (None,[]) ;
};;

let initial_state my_name= 
   let _=Hex_persistent.initialize_all_data_if_necessary () in 
{
   Hex_state_t.role_played = my_name;
   Hex_state_t.config_remains = ((Hex_end_strategy_factory.compute_all_end_configs Hex_persistent.wes_pair)) ;
   Hex_state_t.games_remains = (!(Hex_persistent.games_ref)) ;
   Hex_state_t.openings_remains = (!(Hex_persistent.strong_openings_ref));
   Hex_state_t.moves_before = [];
   Hex_state_t.strong_moves_before = (None,[]) ;
};;

exception No_moves_to_choose_from;;

let compute_usual_move (condition,easy_advances,strong_moves,already_used_moves,moves_before) =
  let opt1=Hex_cell_set.optional_min(easy_advances) in 
  if opt1<>None then Option.unpack opt1 else 
  let opt2=Hex_cell_set.optional_min(strong_moves) in 
  if opt2<>None then Option.unpack opt2 else 
  let opt3=Hex_cell_set.optional_min(already_used_moves) in 
  if opt3<>None then Option.unpack opt3 else 
  let dim=Hex_persistent.dimension () in 
  let remaining_world = Hex_cell_set.apply_condition condition (Hex_common.all_cells dim) in
  let free_cells=Hex_cell_set.setminus remaining_world (Hex_cell_set.safe_set moves_before) in 
  let opt4=Hex_cell_set.optional_min(free_cells) in 
  if opt4<>None then Option.unpack opt4 else 
  raise(No_moves_to_choose_from);;  


let analize sta=
  let player = Hex_common.next_one_to_play (sta.Hex_state_t.moves_before) in 
  let dangers = Hex_fles_double_list.immediate_dangers player sta.Hex_state_t.config_remains in 
  let condition = (
     match dangers with 
     []->None
     |_->Some(Hex_cell_set.fold_intersect (Image.image fst dangers))
  ) in 
  let (unconditioned_strong_moves,unconditioned_used_moves)=
      Hex_fg_double_list.suggested_moves player sta.Hex_state_t.games_remains in 
  let easy_advances = Hex_cell_set.safe_set(Hex_so_list.easy_advances sta.Hex_state_t.openings_remains) in 
  let strong_moves1=Hex_cell_set.apply_condition condition unconditioned_strong_moves 
  and used_moves1=Hex_cell_set.apply_condition condition unconditioned_used_moves in 
  let strong_moves = Hex_cell_set.setminus strong_moves1 easy_advances in 
  let used_moves = Hex_cell_set.setminus used_moves1 easy_advances in 
  let u_move = compute_usual_move (condition,easy_advances,strong_moves,used_moves,sta.Hex_state_t.moves_before) in 
  let (d1,d2) = Hex_fles_double_list.sizes sta.Hex_state_t.config_remains in 
  let (d3,d4) = Hex_fg_double_list.sizes sta.Hex_state_t.games_remains in 
  let d5 = List.length (sta.Hex_state_t.openings_remains) in 
  {
     Hex_analysis_result_t.mandatory_set = condition ;
     involved_end_strategies = Image.image snd dangers ;
     easy_advances = easy_advances ;
     strong_moves = strong_moves ;
     already_used_moves = used_moves ;
     usual_move = u_move;
     sizes = (d1,d2,d3,d4,d5);
  } ;;

let absorb_move sta cell=
   let player = Hex_common.next_one_to_play (sta.Hex_state_t.moves_before) in 
   let ana = analize sta in 
   let old_smb= sta.Hex_state_t.strong_moves_before in 
   let new_smb=(
       let (opt,mb)=old_smb in 
       match opt with 
      None->if Hex_cell_set.mem cell ana.Hex_analysis_result_t.strong_moves
            then (None,cell::mb)
            else (Some(cell),mb)
     |Some(_)->old_smb     
   ) in
   {
      sta with
      Hex_state_t.config_remains = Hex_fles_double_list.simplify_by_move (player,cell) sta.Hex_state_t.config_remains ;
      Hex_state_t.games_remains = Hex_fg_double_list.simplify_by_move cell sta.Hex_state_t.games_remains ;
      Hex_state_t.openings_remains = Hex_so_list.simplify_by_move cell sta.Hex_state_t.openings_remains ;
      Hex_state_t.moves_before =  (cell::(sta.Hex_state_t.moves_before)) ;
      Hex_state_t.strong_moves_before = new_smb;   
   };;



  

