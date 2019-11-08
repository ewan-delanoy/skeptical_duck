(* 

#use"Hex_analysis/hex_state.ml";;

*)

let empty_state = 
{
   Hex_state_t.config_remains = Hex_fles_double_list.empty_one ;
   Hex_state_t.games_remains = Hex_fg_double_list.empty_one ;
   Hex_state_t.openings_remains = [] ;
   Hex_state_t.moves_before = [] ;
   Hex_state_t.strong_moves_before = (None,[]) ;
};;

let initial_state ()= 
   let _=Hex_persistent.initialize_all_data_if_necessary () in 
{
   Hex_state_t.config_remains = ((Hex_end_strategy_factory.compute_all_end_configs Hex_persistent.wes_pair)) ;
   Hex_state_t.games_remains = (!(Hex_persistent.games_ref)) ;
   Hex_state_t.openings_remains = (!(Hex_persistent.untamed_openings_ref));
   Hex_state_t.moves_before = [];
   Hex_state_t.strong_moves_before = (None,[]) ;
};;

exception No_moves_to_choose_from;;

let compute_usual_move (condition,easy_advancer,strong_moves,moves_before) =
  let opt1=easy_advancer in 
  if opt1<>None then fst(Option.unpack opt1) else 
  let opt2=Hex_cell_set.optional_min(strong_moves) in 
  if opt2<>None then Option.unpack opt2 else 
  let dim=Hex_persistent.dimension () in 
  let remaining_world = Hex_cell_set.apply_condition condition (Hex_common.all_cells dim) in
  let free_cells=Hex_cell_set.setminus remaining_world (Hex_cell_set.safe_set moves_before) in 
  let opt3=Hex_cell_set.optional_min(free_cells) in 
  if opt3<>None then Option.unpack opt3 else 
  raise(No_moves_to_choose_from);;  



exception Disjunction_found of int* (int list);;

let analize sta=
  let player = Hex_common.next_one_to_play (sta.Hex_state_t.moves_before) in 
  let dangers = Hex_fles_double_list.immediate_dangers player sta.Hex_state_t.config_remains in 
  let condition = (
     match dangers with 
     []->None
     |_->Some(Hex_cell_set.fold_intersect (Image.image fst dangers))
  ) in 
  if condition = Some(Hex_cell_set_t.S[])
  then raise(Disjunction_found(List.length sta.Hex_state_t.moves_before,Image.image snd dangers))
  else 
  let unconditioned_strong_moves=
      Hex_fg_double_list.suggested_moves player sta.Hex_state_t.games_remains in 
  let easy_advancer = Hex_uog_list.seek_interesting_move sta.Hex_state_t.openings_remains in
  let easy_advances= (
      match easy_advancer with 
       None -> Hex_cell_set.safe_set []
      |Some(cell,_)->Hex_cell_set.safe_set [cell]
  ) in
  let strong_moves1=Hex_cell_set.apply_condition condition unconditioned_strong_moves  in 
  let strong_moves = Hex_cell_set.setminus strong_moves1 easy_advances in 
  let u_move = compute_usual_move (condition,easy_advancer,strong_moves,sta.Hex_state_t.moves_before) in 
  let enem = Hex_fles_double_list.number_of_enemy_strategies player sta.Hex_state_t.config_remains in 
  {
     Hex_analysis_result_t.next_to_play = player ; 
     mandatory_set = condition ;
     involved_end_strategies = Image.image snd dangers ;
     easy_advancer = easy_advancer ;
     strong_moves = strong_moves ;
     usual_move = u_move;
     number_of_remaining_enemies = enem;
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
      
      Hex_state_t.config_remains = Hex_fles_double_list.simplify_by_move (player,cell) sta.Hex_state_t.config_remains ;
      Hex_state_t.games_remains = Hex_fg_double_list.simplify_by_move cell sta.Hex_state_t.games_remains ;
      Hex_state_t.openings_remains = Hex_uog_list.simplify_by_move cell sta.Hex_state_t.openings_remains ;
      Hex_state_t.moves_before =  (cell::(sta.Hex_state_t.moves_before)) ;
      Hex_state_t.strong_moves_before = new_smb;   
   };;

