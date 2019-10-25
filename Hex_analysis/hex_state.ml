(* 

#use"Hex_analysis/hex_state.ml";;

*)

let empty_state = 
{
   Hex_state_t.whoami = Hex_player_t.First_player; (* random decision, should never be used *)
   Hex_state_t.config_remains = Hex_fles_double_list.empty_one ;
   Hex_state_t.games_remains = Hex_fg_double_list.empty_one ;
   Hex_state_t.moves_before = [] ;
   Hex_state_t.still_strong = true;
   Hex_state_t.strong_moves_before = [] ;
};;

let initial_state my_name= 
   let _=Hex_persistent.retrieve_all_data () in 
{
   Hex_state_t.whoami = my_name;
   Hex_state_t.config_remains = ((Hex_end_strategy_factory.compute_all_end_configs Hex_persistent.wes_pair)) ;
   Hex_state_t.games_remains = (!(Hex_persistent.games_ref)) ;
   Hex_state_t.moves_before = [];
   Hex_state_t.still_strong = true; 
   Hex_state_t.strong_moves_before = [] ;
};;

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
  let strong_moves=Hex_cell_set.apply_condition condition unconditioned_strong_moves 
  and used_moves=Hex_cell_set.apply_condition condition unconditioned_used_moves in 
  {
     Hex_analysis_result_t.mandatory_set = condition ;
     strong_moves = strong_moves ;
     already_used_moves = used_moves
  } ;;

let absorb_move sta cell=
   let player = Hex_common.next_one_to_play (sta.Hex_state_t.moves_before) in 
   let ana = analize sta in 
   let still_strong_now =(
      if sta.Hex_state_t.still_strong 
      then Hex_cell_set.mem cell ana.Hex_analysis_result_t.strong_moves
      else false
   ) in 
   let smb=(
     if still_strong_now 
     then cell :: (sta.Hex_state_t.strong_moves_before)
     else sta.Hex_state_t.strong_moves_before
   ) in 
   {
      sta with
      Hex_state_t.config_remains = Hex_fles_double_list.absorb_move (player,cell) sta.Hex_state_t.config_remains ;
      Hex_state_t.games_remains = Hex_fg_double_list.absorb_move cell sta.Hex_state_t.games_remains ;
      Hex_state_t.moves_before =  (cell::(sta.Hex_state_t.moves_before)) ;
      Hex_state_t.still_strong = still_strong_now;
      Hex_state_t.strong_moves_before = smb;
   };;



  



  