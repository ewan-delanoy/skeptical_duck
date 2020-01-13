(* 

#use"Hex_analysis/hex_old_meeting_result.ml";;


*)

module Private = struct 

let rec helper (moves_before,player,moves_after,fles)=
   let a = Hex_flattened_end_strategy_field.active_part fles in 
   if Hex_cell_set.length a=1
   then  (
           match moves_after with 
           []->Hex_meeting_result_t.Attack_but_no_surrender (
              List.rev(moves_before),Hex_cell_set.min a,0) 
           |next_move::_->
              if (Hex_cell_set.mem next_move (Hex_flattened_end_strategy.support fles))
              then Hex_meeting_result_t.Attack_but_no_surrender (List.rev(moves_before),Hex_cell_set.min a,List.length moves_after) 
              else Hex_meeting_result_t.Surrender(List.rev(moves_before),Hex_cell_set.min a,next_move) 
         )     
   else (
          match moves_after with 
          []->Hex_old_meeting_result_t.Stalemate(a)
          |move::following_moves ->
             (match Hex_flattened_end_strategy.use_move_to_simplify_one (player,move) fles with
               None -> Hex_old_meeting_result_t.Victory(player,move)
               |Some(new_fles) ->
               helper
                 (move::moves_before,Hex_player.other_player player,following_moves,new_fles) 
             ) 
        );;

end;;

let meet fles fgame =
   Private.helper([],Hex_player_t.First_player, 
                   fgame.Hex_old_finished_game_t.sequence_of_moves,fles);; 
  