(* 

#use"Hex_analysis/hex_meeting_result.ml";;


*)

module Private = struct 

let rec helper (moves_before,player,moves_after,fles)=
   let a = fles.Hex_flattened_end_strategy_t.active_part in 
   if Hex_cell_set.length a=1
   then  Hex_meeting_result_t.Relevance(
              List.rev(moves_before),Hex_cell_set.min a) 
   else (
          match moves_after with 
          []->Hex_meeting_result_t.Incomplete(a)
          |move::following_moves ->
             (match Hex_flattened_end_strategy.use_move_to_simplify_one (player,move) fles with
               None -> Hex_meeting_result_t.Separation(player,move)
               |Some(new_fles) ->
               helper
                 (move::moves_before,Hex_player.other_player player,following_moves,new_fles) 
             ) 
        );;

end;;

let meet fles fgame =
   Private.helper([],Hex_player_t.First_player, 
                   fgame.Hex_finished_game_t.sequence_of_moves,fles);; 
  