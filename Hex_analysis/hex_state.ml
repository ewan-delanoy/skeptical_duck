(* 

#use"Hex_analysis/hex_state.ml";;

*)


let initial_state ()= {
   Hex_state_t.config_remains = ((Hex_strategy_factory.compute_all_end_configs Hex_persistent.wes_pair)) ;
   Hex_state_t.games_remains = (!(Hex_persistent.games_ref)) ;
   Hex_state_t.moves_before = [] 
};;

let absorb_move sta cell=
   let player = Hex_common.next_one_to_play (sta.Hex_state_t.moves_before) in 
   {
      Hex_state_t.config_remains = Hex_ec_double_indexed_list.absorb_move (player,cell) sta.Hex_state_t.config_remains ;
      Hex_state_t.games_remains = Hex_fg_double_list.absorb_move cell sta.Hex_state_t.games_remains ;
      Hex_state_t.moves_before = cell::(sta.Hex_state_t.moves_before) ;
   };;

let analize sta=
  let player = Hex_common.next_one_to_play (sta.Hex_state_t.moves_before) in 
  let dangers = Hex_ec_double_indexed_list.immediate_dangers player sta.Hex_state_t.config_remains in 
  let condition = (
     match dangers with 
     []->None
     |_->Some(Hex_cell_set.fold_intersect (Image.image fst dangers))
  ) in 
  let (unconditionned_winning_moves,unconditioned_used_moves)=
      Hex_fg_double_list.suggested_moves player sta.Hex_state_t.games_remains in 
  let winning_moves=Hex_cell_set.apply_condition condition unconditionned_winning_moves 
  and used_moves=Hex_cell_set.apply_condition condition unconditioned_used_moves in 
  {
     Hex_analysis_result_t.mandatory_set = condition ;
     winning_moves = winning_moves ;
     already_used_moves = used_moves
  } ;;
  



  